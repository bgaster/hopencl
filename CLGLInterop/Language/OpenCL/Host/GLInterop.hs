{-# OPTIONS_GHC -XForeignFunctionInterface -cpp -fglasgow-exts #-}
{-# LANGUAGE GADTs #-}

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

module Language.OpenCL.Host.GLInterop where

import Control.Monad.Trans
import Foreign
import Foreign.C
import Graphics.Rendering.OpenGL
import Language.OpenCL.Host
import Language.OpenCL.Host.Core
import Language.OpenCL.Host.FFI

----------------------------------------------------------------------------------------------------
-- FFI layer
----------------------------------------------------------------------------------------------------

#define CLCtor Ptr CLInt -> IO
#define CLGetter SizeT -> Ptr a -> Ptr SizeT -> IO CLInt
#define CLEventDriven CLUInt -> Ptr Event -> Ptr Event -> IO CLInt

----------------------------------------------------------------------------------------------------
-- Windows/WGL stuff

data HGLRC_
type HGLRC = Ptr HGLRC_

data HDC_
type HDC = Ptr HDC_

foreign import stdcall "wingdi.h wglGetCurrentContext" wglGetCurrentContext :: IO HGLRC
foreign import stdcall "wingdi.h wglGetCurrentDC" wglGetCurrentDC :: IO HDC

----------------------------------------------------------------------------------------------------
-- OpenCL extensions

foreign import stdcall "cl.h clGetExtensionFunctionAddress" clGetExtensionFunctionAddress :: CString -> IO (FunPtr t)

----------------------------------------------------------------------------------------------------
-- New context stuff
-- WARNING: Only works on Win32...

data GLInteropContextProperty t
    where GLContext :: GLInteropContextProperty HGLRC
          WGLHDC    :: GLInteropContextProperty HDC

instance Const (GLInteropContextProperty t) CLContextProperty
    where value GLContext = 0x2008
          value WGLHDC    = 0x200B

instance ContextProperty GLInteropContextProperty t

type CLGLContextInfo = CLUInt

-- This is based on the way the OpenGLRaw package works, but not using their level of cpp magic.
-- Hopefully I don't have to do it regularly enough to make the cpp hackery worthwhile...

foreign import stdcall "dynamic" mk_clGetGLContextInfoKHR :: FunPtr (Ptr CLContextProperty -> CLGLContextInfo -> CLGetter) -> (Ptr CLContextProperty -> CLGLContextInfo -> CLGetter)

clGetGLContextInfoKHR :: Ptr CLContextProperty -> CLGLContextInfo -> CLGetter
clGetGLContextInfoKHR = mk_clGetGLContextInfoKHR (unsafePerformIO (withCString "clGetGLContextInfoKHR" clGetExtensionFunctionAddress))

-- Wrapper

data GLContextInfo t 
    where CurrentDeviceForGLContext :: GLContextInfo Device
          DevicesForGLContext :: GLContextInfo [Device]

instance Const (GLContextInfo t) CLGLContextInfo
    where value CurrentDeviceForGLContext = 0x2006
          value DevicesForGLContext       = 0x2007

getGLContextInfo :: forall t. Context -> GLContextInfo t -> IO t
getGLContextInfo context info =
    do cprops <- context ? ContextProperties
       withContextProperties cprops $ \ cpropsptr -> 
           (case info of 
              CurrentDeviceForGLContext -> getStorable (clGetGLContextInfoKHR cpropsptr (value info))
              DevicesForGLContext       -> getArray (clGetGLContextInfoKHR cpropsptr (value info))) :: IO t

----------------------------------------------------------------------------------------------------
-- Buffers

foreign import stdcall "cl_gl.h clCreateFromGLBuffer" clCreateFromGLBuffer :: Context -> CLMemFlags -> GLuint -> CLCtor CLMem


createFromGLBuffer :: Context -> BitSet MemFlag -> BufferObject -> IO (Buffer t)
createFromGLBuffer ctxt memFlags (BufferObject glBuffer) =
    Buffer `fmap` handleCLCreationError "createFromGLBuffer" (clCreateFromGLBuffer ctxt (valueFrom memFlags) glBuffer)

----------------------------------------------------------------------------------------------------
-- Acquiring and releasing GL objects

foreign import stdcall "cl_gl.h clEnqueueAcquireGLObjects" clEnqueueAcquireGLObjects :: CommandQueue -> CLUInt -> Ptr CLMem -> CLEventDriven
foreign import stdcall "cl_gl.h clEnqueueReleaseGLObjects" clEnqueueReleaseGLObjects :: CommandQueue -> CLUInt -> Ptr CLMem -> CLEventDriven

enqueueAcquireGLObjects :: CommandQueue -> [Buffer t] -> [Event] -> IO Event
enqueueAcquireGLObjects queue buffers waitEvents =
    withArray (map unBuffer buffers) $ \ ptr ->
        handleCLEventDriven "enqueueAcquireGLObjects" (clEnqueueReleaseGLObjects queue (fromIntegral $ length buffers) ptr) waitEvents

enqueueReleaseGLObjects :: CommandQueue -> [Buffer t] -> [Event] -> IO Event
enqueueReleaseGLObjects queue buffers waitEvents = 
    withArray (map unBuffer buffers) $ \ ptr ->
        handleCLEventDriven "enqueueReleaseGLObjects" (clEnqueueReleaseGLObjects queue (fromIntegral $ length buffers) ptr) waitEvents

----------------------------------------------------------------------------------------------------
-- Idiomatic wrappers
----------------------------------------------------------------------------------------------------

instance Queryable Context GLContextInfo
    where context ? info = liftIO $ getGLContextInfo context info

bufferFromGLBuffer :: Contextual m => BufferObject -> m (Buffer t)
bufferFromGLBuffer glBuffer = 
    contextual $ do context <- theContext; liftIO (createFromGLBuffer context (bitSet []) glBuffer)

bufferFromGLBufferWithFlags :: Contextual m => BufferObject -> [MemFlag] -> m (Buffer t)
bufferFromGLBufferWithFlags glBuffer memFlags = 
    contextual $ do context <- theContext; liftIO (createFromGLBuffer context (bitSet memFlags) glBuffer)

acquireGLObjects, releaseGLObjects :: Queued m => [Buffer t] -> [Event] -> m Event
acquireGLObjects buffers waitEvents = 
    queued $ do queue <- theQueue; liftIO (enqueueAcquireGLObjects queue buffers waitEvents)
releaseGLObjects buffers waitEvents = 
    queued $ do queue <- theQueue; liftIO (enqueueReleaseGLObjects queue buffers waitEvents)

withGLObjects :: (Queued m, CatchIO m) => [Buffer t] -> m u -> m u
withGLObjects buffers comp = 
    do acquireGLObjects buffers []
       comp `finally` releaseGLObjects buffers []
