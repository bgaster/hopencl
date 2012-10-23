{-# OPTIONS_GHC -cpp -fglasgow-exts -XNamedFieldPuns #-}

{-
Copyright ©2012 Advanced Micro Devices, Inc. All rights reserved.

*   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
*   Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.  
*   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
********************************************************************

-}

module Render where

import Data.Array.Storable
import Data.IORef
import Data.List (mapAccumL)
import Data.Maybe (catMaybes)
import Data.Time.Clock
import Data.Word
import Graphics.UI.GLUT hiding (finish, ReadWrite)
import Language.OpenCL.Host
import Language.OpenCL.Host.GLInterop

import BaseTypes
import qualified Device

data GlobalRenderState = GRS { localStates :: [Device.RenderState]
                             , scene :: Scene [Sphere]
                             , primaryQueue :: CommandQueue
                             , globalPixels :: Buffer Word 
                             , globalContext :: Context }

instance MonadIO m => Wraps GlobalRenderState QueueM m
    where with grs comp = with (globalContext grs) $ with (primaryQueue grs) comp

-- Note: we'll use the current GL device for rendering, even if it's not in the device list...
newGRS :: (CatchIO m, Contextual m) => Camera -> Scene [Sphere] -> Word32 -> [Device] -> m GlobalRenderState
newGRS camera scene preferredWorkGroupSize allDevices = 
    do puts "[init] newGRS"
       globalContext <- theContext

       -- Create devices       
       primaryDevice <- queryContext CurrentDeviceForGLContext
       let ds = filter (primaryDevice /=) allDevices
       primaryQueue <- queueWithProperties primaryDevice [Profiling]
       globalPixels <- buffer (fromIntegral nPixels)

       puts "[init] generating devices"
       primaryDevice <- Device.new primaryDevice preferredWorkGroupSize
       secondaryDevices <- mapM (flip Device.new preferredWorkGroupSize) ds

       -- No performance information yet, so we'll just assign work equally
       let nDevices = fromIntegral $ length secondaryDevices + 1
           nRows = floor (fromIntegral (height scene) / fromIntegral nDevices)
           rowAmounts = if nRows * nDevices == height scene
                        then [(nRows * i, nRows) | i <- [0..nDevices - 1]]
                        else (0, nRows + (height scene - nRows * nDevices)) : [(nRows * i + 1, nRows) | i <- [1..nDevices - 1]]
           workAmounts = map (\(offset, amount) -> (offset * width scene, amount * width scene)) rowAmounts

       puts "[init] generating local render states"
       localStates <- sequence [ Device.newRenderState device camera scene offset amount globalPixels
                                 | device <- primaryDevice : secondaryDevices
                                 | (offset, amount) <- workAmounts ]
       
       return GRS { localStates
                  , scene
                  , primaryQueue
                  , globalPixels 
                  , globalContext }

    where nPixels = width scene * height scene

----------------------------------------------------------------------------------------------------
-- Work loads

recomputeWorkLoads :: (Queued m, CatchIO m) => GlobalRenderState -> m GlobalRenderState
recomputeWorkLoads grs = 
    do efficiencies <- mapM Device.efficiency (localStates grs)
       localStates <- sequence [ Device.updateWork (offset * w) (amount * w) ls | ls <- localStates grs | (offset, amount) <- splitWork efficiencies ]
       return grs { localStates }
    where nRows = height (scene grs)
          w = width (scene grs)
          splitWork efficiencies = [(offset, amount) | offset <- sums | amount <- secondPass ]
              where total = sum efficiencies
                    normed = [ e / total | e <- efficiencies ]
                    firstPass = map (floor . (fromIntegral nRows *)) normed
                    secondPass | distributed == nRows = firstPass
                               | otherwise            = (first + (nRows - distributed)) : rest
                        where distributed = sum firstPass
                              (first:rest) = firstPass
                    sums = scanl (+) 0 secondPass

----------------------------------------------------------------------------------------------------
-- Rendering

render :: (Queued m, CatchIO m) => GlobalRenderState -> Word -> m (GlobalRenderState, Word)
render grs iteration 
    | iteration <= 20 = onePass grs iteration
    | otherwise       = do startTime <- liftIO (getCurrentTime)
                           loop grs iteration (addUTCTime (fromIntegral (min (iteration - 20) 100) / 100 * 0.5) startTime)
    where onePass grs iteration = do mapM_ (Device.render iteration) (localStates grs)
                                     mapM_ (\ls -> with ls finish) (localStates grs)
                                     return (grs, iteration + 1)
          loop grs iteration endTime = 
              do now <- liftIO getCurrentTime
                 if now < endTime
                    then do (grs, iteration) <- onePass grs iteration
                            loop grs iteration endTime
                    else return (grs, iteration)

