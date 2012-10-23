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

module Device where

import Control.Monad
import Control.Monad.Trans (MonadIO)
import Data.Array.Storable hiding (range)
import Data.IORef
import Data.Maybe (isJust)
import Data.Time.Clock
import Data.Word
import Foreign.Ptr
import Foreign.Storable

import Language.OpenCL.Host

import System.Random

import BaseTypes


data RenderState = RenderState { camera :: Buffer Camera
                               , scene :: Scene (Buffer Sphere)
                               , device :: RenderDevice
                               , offset, amount :: Word
                               , seeds :: Buffer Word
                               , workBuffers :: Buffers
                               , globalPixels :: Buffer Word }

data RenderDevice = RD { deviceQueue :: CommandQueue
                       , deviceKernel :: Kernel
                       , deviceName :: String
                       , workGroupSize :: Word
                       , timing :: IORef Timing }

data Timing = Timing { count :: Word
                     , time :: Double }

instance Contextual m => Wraps RenderDevice QueueM m
    where with rd comp = with (deviceQueue rd) comp
instance Contextual m => Wraps RenderState QueueM m
    where with rs = with (device rs)

----------------------------------------------------------------------------------------------------
-- Construction

new :: Contextual m => Device -> Word32 -> m RenderDevice
new device preferredWorkGroupSize =
    do n <- device ? DeviceName
       q <- queueWithProperties device [Profiling]
       p <- programFromSource =<< liftIO (readFile "HCLRayTrace/rendering_kernel.cl")
       buildProgram p [device] ""
       k <- kernel p "RadianceGPU"
       dMaxWGS <- device ? DeviceMaxWorkGroupSize
       kWGS <- (k, device) ? KernelWorkGroupSize
       timingRef <- liftIO (newIORef (Timing 0 0))
       return RD { deviceQueue = q
                 , deviceKernel = k
                 , deviceName = n
                 , workGroupSize = fromIntegral $ minimum [preferredWorkGroupSize, dMaxWGS, kWGS]
                 , timing = timingRef }

newRenderState :: Contextual m => RenderDevice -> Camera -> Scene [Sphere] -> Word -> Word -> Buffer Word -> m RenderState
newRenderState device camera scene offset amount globalPixels =
    with device $
        do cameraBuffer <- buffer 1
           writeTo cameraBuffer 0 [camera]
           spheresBuffer <- bufferWithFlags (fromIntegral $ sphereCount scene) [ReadOnly]
           writeTo spheresBuffer 0 (spheres scene)
           (seeds :: StorableArray Int Word) <-
               liftIO $ do rs <- newStdGen
                           newListArray (0, amount' * 2 - 1) (map fromIntegral (randomRs (2, maxBound :: Int) rs))
           seedBuffer <- bufferWithFlags (amount' * 2) [ReadWrite]
           writeTo seedBuffer 0 seeds
           workBuffers <- do workColors <- bufferWithFlags amount' [ReadWrite]
                             workPixels <- bufferWithFlags amount' [ReadWrite]
                             return (Buffers workColors workPixels)
           return RenderState { camera = cameraBuffer
                              , scene = scene { spheres = spheresBuffer }
                              , device, offset, amount
                              , seeds = seedBuffer
                              , workBuffers
                              , globalPixels }
    where amount' = fromIntegral amount

----------------------------------------------------------------------------------------------------
-- Work loads and efficiency

efficiency :: MonadIO m => RenderState -> m Double
efficiency rs =
    do (Timing count time) <- liftIO (readIORef (timing (device rs)))
       return (fromIntegral count / time)

updateWork :: (Queued m, CatchIO m) => Word -> Word -> RenderState -> m RenderState
updateWork newOffset newAmount rs =
    if newAmount == amount rs
    then return rs { offset = newOffset, amount = newAmount }
    else do puts $ deviceName (device rs) ++ ": updateWork " ++ show newOffset ++ " " ++ show newAmount
            newColors <- bufferWithFlags (amount') [ReadWrite]
            newPixels <- bufferWithFlags (amount') [ReadWrite]
            (seeds :: StorableArray Int Word) <-
                liftIO $ do rs <- newStdGen
                            newListArray (0, amount' * 2 - 1) (map fromIntegral (randomRs (2, maxBound :: Int) rs))
            newSeeds <- bufferWithFlags (amount' * 2) [ReadWrite]
            writeTo newSeeds 0 seeds
            return rs { offset = newOffset
                      , amount = newAmount
                      , seeds = newSeeds
                      , workBuffers = Buffers newColors newPixels }
    where amount' = fromIntegral newAmount


----------------------------------------------------------------------------------------------------
-- Rendering

render :: Queued m => Word -> RenderState -> m Event
render iteration (RenderState camera scene device offset amount seeds (Buffers workColors workPixels) globalPixels) =
    with device $
    do before <- liftIO getCurrentTime
       ev <- invoke k workColors
                      seeds
                      (spheres scene)
                      camera
                      (sphereCount scene)
                      (width scene)
                      (height scene)
                      iteration
                      workPixels
                      offset
                      amount
             `overRange` ([], [fromIntegral amount'], [fromIntegral (workGroupSize device)])
       setDatalessEventCallback ev (callback before)
       copyBuffer workPixels globalPixels 0 (fromIntegral offset) (fromIntegral amount)
    where amount' = if amount `mod` workGroupSize device == 0
                    then amount
                    else (amount `div` workGroupSize device + 1) * workGroupSize device
          k = deviceKernel device
          callback before _ status _ =
              when (status == Complete) $
                  do after <- getCurrentTime
                     modifyIORef (timing device) (\(Timing count time) -> Timing (count + amount) (time + realToFrac (diffUTCTime after before)))
