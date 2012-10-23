{-# OPTIONS_GHC -fglasgow-exts -XNamedFieldPuns #-}

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

module Main where

import Control.Monad
import Data.Array.Storable
import Data.IORef
import Data.Time.Clock
import Data.Word
import Foreign.Ptr (nullPtr)
import Foreign.Storable (sizeOf)
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT hiding (finish, readBuffer, ReadWrite, WriteOnly)
import Language.OpenCL.Host hiding (Char, RGBA)
import Language.OpenCL.Host.Core (unBuffer)
import Language.OpenCL.Host.GLInterop
import System.Exit

import BaseTypes
import qualified Device as Device
import Render
import Scene


baseCaption = "HCLRayTracer v 0.1 (based upon SmallptGPU v2.0, written by David Bucciarelli)"

testPattern :: Int -> [Vector4 GLbyte]
testPattern width = cycle (take width (cycle (reds ++ greens ++ blues)))
    where reds = [Vector4 i 0 0 1 | i <- [0..maxBound :: GLbyte]]
          greens = [Vector4 0 i 0 1 | i <- [0..maxBound :: GLbyte]]
          blues = [Vector4 0 0 i 1 | i <- [0..maxBound :: GLbyte]]

sceneSizeInBytes width height = fromIntegral $ width * height * fromIntegral (sizeOf (undefined :: Vector4 GLbyte))

updateFPS = 1

main = do (_, args) <- getArgsAndInitialize
          initialDisplayMode $= [ RGBMode, DoubleBuffered ]
          initialWindowPosition $= Position 0 0
          initialWindowSize $= Size 800 600
          createWindow baseCaption

          ps <- platforms
          amdPlatforms <- filterM (\p -> (`elem` amdVendorStrings) `fmap` (p ? PlatformVendor)) ps
          let platform = if null amdPlatforms then head ps else head amdPlatforms
          allDevices <- devices platform

          currentContext <- wglGetCurrentContext
          currentDC <- wglGetCurrentDC
          let cprops = pushContextProperty ContextPlatform platform $
                       pushContextProperty GLContext currentContext $
                       pushContextProperty WGLHDC currentDC noProperties
          c <- contextFromProperties cprops allDevices

          [pbo] <- genObjectNames 1
          bindBuffer PixelUnpackBuffer $= Just pbo
          testPattern <- newListArray (0, (800*600 - 1)) (testPattern 800)
          withStorableArray testPattern $ \ ptr -> bufferData PixelUnpackBuffer $= (sceneSizeInBytes 800 600, ptr, DynamicDraw)
          drawPixels (Size 800 600) (PixelData RGBA UnsignedByte nullPtr)
          swapBuffers

          (camera, spheres) <- readScene 800 600 (if null args then "HCLRayTrace/scenes/cornell.scn" else args !! 0)
          let scene = Scene { width = 800
                            , height = 600
                            , sphereCount = fromIntegral $ length spheres
                            , spheres }
          grs <- with c $ newGRS camera scene (fromIntegral $ 800 `div` 4)
                 (if null args || null (tail args) || args !! 1 == "all" then allDevices else [])

          stateRef <- newIORef (camera, scene, grs, 0)

          matrixMode $= Projection
          viewport $= (Position 0 0, Size 800 600)
          loadIdentity
          ortho 0 (800 - 1) 0 (600 - 1) (-1) 1
          now <- getCurrentTime
          frameCountRef <- newIORef (0, now)

          displayCallback $= display stateRef frameCountRef
          idleCallback $= Just (updateRendering c stateRef)
          keyboardMouseCallback $= Just (keyMouse c stateRef)
          -- reshapeCallback $= Just (reshape c stateRef)

          putStrLn "[init] done"

          mainLoop

----------------------------------------------------------------------------------------------------
-- Callbacks

-- reshape c stateRef (Size width' height') =
--     do (camera, scene, grs, iter) <- readIORef stateRef
--        when (width scene /= fromIntegral width' || height scene /= fromIntegral height') $
--             do grs' <- with c $ with grs $ updateDimensions (fromIntegral width') (fromIntegral height') grs
--               writeIORef stateRef (camera, scene { width = fromIntegral width', height = fromIntegral height' }, grs', 0)
--               updateRendering c stateRef

keyMouse _ _ (Char 'q') Down _ _ = exitWith ExitSuccess
keyMouse c stateRef (Char 'l') Down _ _ =
    do (camera, scene, grs, _) <- readIORef stateRef
       grs' <- with c $ with grs $ recomputeWorkLoads grs
       writeIORef stateRef (camera, scene, grs', 0)
keyMouse _ stateRef (Char 'r') Down _ _ =
    do (camera, scene, grs, _) <- readIORef stateRef
       writeIORef stateRef (camera, scene, grs, 0)
keyMouse _ _ _ _ _ _             = return ()

display stateRef frameCountRef =
    do (_, scene, grs, iter) <- readIORef stateRef
       rasterPos (Vertex2 0 (0 :: GLint))
       drawPixels (size scene) (PixelData RGBA UnsignedByte nullPtr)
       swapBuffers

       errs <- get errors
       when (not (null errs)) (mapM_ print errs)

       (frames, lastTime) <- readIORef frameCountRef
       now <- getCurrentTime
       let elapsed = diffUTCTime now lastTime
       if elapsed > updateFPS
          then let fps = round $ (frames + 1) / elapsed
               in do windowTitle $= baseCaption ++ " | Pass " ++ show iter ++ " | " ++ show fps ++ " FPS"
                     writeIORef frameCountRef (0, now)
          else writeIORef frameCountRef (frames + 1, lastTime)

    where size scene = Size (fromIntegral $ width scene) (fromIntegral $ height scene)

updateRendering :: Context -> IORef (Camera, Scene [Sphere], GlobalRenderState, Word) -> IO ()
updateRendering c stateRef =
    do (camera, scene, grs, iter) <- readIORef stateRef
       (grs', iter') <- with c $ with grs $ render grs iter
       writeIORef stateRef (camera, scene, grs', iter')
       (thePixels :: StorableArray Int Word) <-
           with c $ with grs' $ readFrom (globalPixels grs) 0 (fromIntegral $ width scene * height scene)
       withStorableArray thePixels $ \ pixelsPtr -> bufferData PixelUnpackBuffer $= (sceneSizeInBytes (width scene) (height scene), pixelsPtr, DynamicDraw)
       postRedisplay Nothing
