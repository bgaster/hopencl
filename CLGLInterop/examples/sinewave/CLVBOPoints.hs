{-# OPTIONS_GHC -XBangPatterns -O2 #-}

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

module CLVBOPoints where

import Codec.BMP as Bmp
import Control.Arrow (first, second)
import Control.Monad (filterM, when)
import Control.Monad.Trans (liftIO)
import Data.Array.Storable hiding (range)
import Data.ByteString.Unsafe as BS
import Data.IORef
import Data.Time.Clock
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT hiding (finish, WriteOnly)
import Foreign (sizeOf, nullPtr, plusPtr)
import System.Exit

import Language.OpenCL.Host hiding (CLBaseType(..), RGBA)
import Language.OpenCL.Host.GLInterop

meshWidth = 512
meshHeight = 512
nVertices = meshWidth * meshHeight

size = meshWidth * meshHeight * 8 * sizeOf (undefined :: GLfloat)

--

animRate :: Float
animRate = 0.01

updateFPS = 1
desiredGroupSize = 256

--

main = do -- GLUT setup
          (_, args) <- getArgsAndInitialize
          initialDisplayMode $= [ RGBMode, DoubleBuffered, WithSamplesPerPixel 4 ]
          initialWindowPosition $= Position 0 0
          initialWindowSize $= Size 500 500
          createWindow "SimpleGL"

          clearColor $= Color4 0.5 0.5 0.5 1
          viewport $= (Position 0 0, Size 500 500)
          matrixMode $= Projection
          loadIdentity
          perspective 60 1 1 20
          matrixMode $= Modelview 0
          pointSize $= 3

          setupTextures
          loadTexture "sinewave/ATIStream.bmp"

          -- CL setup

          -- Find (preferably AMD) platform:
          ps <- platforms
          when (null ps) (error "No CL platforms available")
          amdPlatforms <- filterM (\p -> (`elem` amdVendorStrings) `fmap` (p ? PlatformVendor)) ps
          let p = if null amdPlatforms then head ps else head amdPlatforms
                  
          -- Setup context
          hdc <- wglGetCurrentDC
          glCtx <- wglGetCurrentContext
          let cprops = pushContextProperty ContextPlatform p $
                       pushContextProperty GLContext glCtx $ 
                       pushContextProperty WGLHDC hdc noProperties
          c <- contextFromPropertiesAndType cprops [GPU]
          (d:_) <- c ? ContextDevices
          putStrLn =<< d ? DeviceName
          theQueue <- with c $ queue d

          -- Setup buffer

          [glBuffer] <- genObjectNames 1
          bindBuffer ArrayBuffer $= Just glBuffer
          ones <- newArray (0,size - 1) (1 :: Int)
          withStorableArray ones $ \ ptr -> 
              bufferData ArrayBuffer $= (fromIntegral size, ptr, DynamicDraw)
          clBuffer <- with c $ bufferFromGLBufferWithFlags glBuffer [WriteOnly]

          -- OpenCL program
          k <- with c $ do program <- programFromSource =<< liftIO (readFile "sinewave/Sinewave_Kernels.cl")
                           buildProgram program [d] ""
                           kernel program "sineWave"

          -- Kernel setup
          fixArgument k 0 clBuffer
          fixArgument k 1 meshWidth
          fixArgument k 2 meshHeight

          -- check group size
          kWorkGroupSize <- (k, d) ? KernelWorkGroupSize
          dWorkGroupSize <- d ? DeviceMaxWorkGroupSize
          let groupSize = minimum [ desiredGroupSize, kWorkGroupSize, dWorkGroupSize ]
          
          rotateRef <- newIORef (0,0)
          animRef <- newIORef (0 :: Float)
          now <- getCurrentTime
          frameCountRef <- newIORef (0, now)

          keyboardMouseCallback $= Just (keymouse rotateRef)
          displayCallback $= display glBuffer (with c . with theQueue . runKernel k clBuffer groupSize) rotateRef animRef frameCountRef
          idleCallback $= Just (postRedisplay Nothing)

          mainLoop

loadTexture :: String -> IO ()
loadTexture fname = 
    do mbitmap <- Bmp.readBMP fname
       case mbitmap of
         Left err -> fail $ "Failed to read bitmap: " ++ show err
         Right bitmap -> 
             let (x, y) = bmpDimensions bitmap 
             in BS.unsafeUseAsCString (Bmp.unpackBMPToRGBA32 bitmap) $ \ bits -> 
                texImage2D Nothing
                           NoProxy
                           0
                           RGBA8
                           (TextureSize2D (fromIntegral x) (fromIntegral y))
                           0
                           (PixelData RGBA UnsignedByte bits)

setupTextures :: IO ()
setupTextures =
    do textureWrapMode Texture2D S $= (Repeated, Repeat)
       textureWrapMode Texture2D T $= (Repeated, Repeat)
       textureFilter Texture2D $= ((Linear', Nothing), Linear')
       textureFunction $= Decal
       texture Texture2D $= Enabled


keymouse rotateRef (Char 'q') _ _ _ = exitWith ExitSuccess
keymouse rotateRef (Char 'a') Down _ _ = do modifyIORef rotateRef (first (+ 2.5))
                                            postRedisplay Nothing
keymouse rotateRef (Char 'd') Down _ _ = do modifyIORef rotateRef (first (subtract 2.5))
                                            postRedisplay Nothing
keymouse rotateRef (Char 'w') Down _ _ = do modifyIORef rotateRef (second (+ 2.5))
                                            postRedisplay Nothing
keymouse rotateRef (Char 's') Down _ _ = do modifyIORef rotateRef (second (subtract 2.5))
                                            postRedisplay Nothing
keymouse _ (Char 'x') Down _ _         = do x <- get (texture Texture2D)
                                            case x of
                                              Enabled -> texture Texture2D $= Disabled
                                              Disabled -> texture Texture2D $= Enabled
                                            postRedisplay Nothing
keymouse _ _ _ _ _                  = return ()

runKernel k clBuffer groupSize animate = 
    withGLObjects [clBuffer] $ do invoke k animate `overRange` ([], [meshWidth, meshHeight], [fromIntegral groupSize, 1])
                                  finish
       
display glBuffer loadBuffer rotateRef animRef frameCountRef = 
    do clear [ ColorBuffer ]
       (rotateY, rotateX) <- readIORef rotateRef
       loadIdentity
       color (Color3 1 0 0 :: Color3 GLfloat)
       translate (Vector3 0 0 (-5) :: Vector3 GLfloat)
       rotate rotateY (Vector3 0 1 0 :: Vector3 GLfloat)
       rotate rotateX (Vector3 1 0 0 :: Vector3 GLfloat)

       animate <- readIORef animRef
       loadBuffer animate

       clientState VertexArray $= Enabled
       clientState ColorArray $= Enabled
       clientState TextureCoordArray $= Enabled

       arrayPointer VertexArray $= VertexArrayDescriptor 3 Float stride (offsetFloats 0)
       arrayPointer ColorArray $= VertexArrayDescriptor 3 Float stride (offsetFloats 3)
       arrayPointer TextureCoordArray $= VertexArrayDescriptor 2 Float stride (offsetFloats 6)
       drawArrays Points 0 (fromIntegral $ nVertices)
       GLUT.finish
       swapBuffers

       clientState VertexArray $= Disabled
       clientState ColorArray $= Disabled
       clientState TextureCoordArray $= Disabled

       writeIORef animRef (animate + animRate)

       (frames, lastTime) <- readIORef frameCountRef
       now <- getCurrentTime
       let elapsed = diffUTCTime now lastTime
       if elapsed > updateFPS
          then let fps = round $ (frames + 1) / elapsed
               in do windowTitle $= "SimpleGL | " ++ show fps ++ " FPS"
                     writeIORef frameCountRef (0, now)
          else writeIORef frameCountRef (frames + 1, lastTime)
    where offsetFloats i = plusPtr nullPtr (sizeOf (undefined :: GLfloat) * i)
          stride = toEnum (sizeOf (undefined :: GLfloat) * 8) 