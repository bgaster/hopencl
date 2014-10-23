{-# OPTIONS_GHC -XForeignFunctionInterface -cpp -fglasgow-exts -w #-}
module Language.OpenCL.Host.FFI where

import Foreign
import Foreign.C

import Language.OpenCL.Host.Constants

-- hsc2hs structure support:
#ifdef __APPLE__
	#include <OpenCL/opencl.h>
#else
	#include "CL/cl.h"
#endif
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

--
-- Common Patterns
--

-- These could be type synonyms, but the GHC FFI doesn't do type synonym expansion before checking
-- whether the results of FFI calls are exportable types.  To work around that, we'll use everyone's
-- favorite tool, the C preprocessor.  ##'s get these through the hsc2hs cpp step.

##define CLCtor Ptr CLInt -> IO
##define CLGetter SizeT -> Ptr a -> Ptr SizeT -> IO CLInt
##define CLCountedArrayGetter(t) CLUInt -> Ptr t -> Ptr CLUInt -> IO CLInt
##define CLEventDriven CLUInt -> Ptr Event -> Ptr Event -> IO CLInt

--
-- 4.1: Querying Platform Info
--

foreign import stdcall "cl.h clGetPlatformIDs" clGetPlatformIDs :: CLCountedArrayGetter(Platform)
foreign import stdcall "cl.h clGetPlatformInfo" clGetPlatformInfo :: Platform -> CLPlatformInfo -> CLGetter

-- 
-- 4.2: Querying Devices
--

foreign import stdcall "cl.h clGetDeviceIDs" clGetDeviceIDs :: Platform -> CLDeviceType -> CLCountedArrayGetter(Device)
foreign import stdcall "cl.h clGetDeviceInfo" clGetDeviceInfo :: Device -> CLUInt -> CLGetter

--
-- 4.3: Contexts
--

foreign import stdcall "cl.h clCreateContext" clCreateContext :: Ptr CLContextProperty -> CLUInt -> Ptr Device -> FunPtr (CString -> Ptr a -> SizeT -> Ptr b -> IO ()) -> Ptr b -> CLCtor Context
foreign import stdcall "cl.h clCreateContextFromType" clCreateContextFromType :: Ptr CInt -> CLDeviceType -> FunPtr (CString -> Ptr a -> SizeT -> Ptr b -> IO ()) -> Ptr b -> CLCtor Context
foreign import stdcall "cl.h clRetainContext" clRetainContext :: Context -> IO CLInt
foreign import stdcall "cl.h clReleaseContext" clReleaseContext :: Context -> IO CLInt
foreign import stdcall "cl.h clGetContextInfo" clGetContextInfo :: Context -> CLContextInfo -> CLGetter

--
-- 5.1: Command Queues
--

foreign import stdcall "cl.h clCreateCommandQueue" clCreateCommandQueue :: Context -> Device -> CLCommandQueueProperties -> CLCtor CommandQueue
foreign import stdcall "cl.h clRetainCommandQueue" clRetainCommandQueue :: CommandQueue -> IO CLInt
foreign import stdcall "cl.h clReleaseCommandQueue" clReleaseCommandQueue :: CommandQueue -> IO CLInt
foreign import stdcall "cl.h clGetCommandQueueInfo" clGetCommandQueueInfo :: CommandQueue -> CLCommandQueueInfo -> CLGetter

--
-- 5.2 Buffer Objects
--

instance Storable CLBufferRegion 
    where alignment _ = #{alignment cl_buffer_region}
          sizeOf _ = #{size cl_buffer_region}
          peek ptr = 
              do origin <- #{peek cl_buffer_region, origin} ptr
                 size <- #{peek cl_buffer_region, size} ptr
                 return (CLBufferRegion origin size)
          poke ptr (CLBufferRegion origin size) =
              do #{poke cl_buffer_region, origin} ptr origin
                 #{poke cl_buffer_region, size} ptr size

foreign import stdcall "cl.h clCreateBuffer" clCreateBuffer :: Context -> CLMemFlags -> SizeT -> Ptr a -> CLCtor CLMem
foreign import stdcall "cl.h clCreateSubBuffer" clCreateSubBuffer :: CLMem -> CLMemFlags -> CLBufferCreateType -> Ptr a -> CLCtor CLMem
foreign import stdcall "cl.h clEnqueueReadBuffer" clEnqueueReadBuffer :: CommandQueue -> CLMem -> Bool -> SizeT -> SizeT -> Ptr a -> CLEventDriven
foreign import stdcall "cl.h clEnqueueWriteBuffer" clEnqueueWriteBuffer :: CommandQueue -> CLMem -> Bool -> SizeT -> SizeT -> Ptr a -> CLEventDriven

foreign import stdcall "cl.h clEnqueueReadBufferRect" clEnqueueReadBufferRect :: 
    CommandQueue -> CLMem -> Bool -> Ptr SizeT -> Ptr SizeT -> Ptr SizeT -> SizeT -> SizeT -> SizeT -> SizeT -> Ptr a -> CLEventDriven
foreign import stdcall "cl.h clEnqueueWriteBufferRect" clEnqueueWriteBufferRect ::
    CommandQueue -> CLMem -> Bool -> Ptr SizeT -> Ptr SizeT -> Ptr SizeT -> SizeT -> SizeT -> SizeT -> SizeT -> Ptr a -> CLEventDriven
foreign import stdcall "cl.h clEnqueueCopyBuffer" clEnqueueCopyBuffer :: CommandQueue -> CLMem -> CLMem -> SizeT -> SizeT -> SizeT -> CLEventDriven
foreign import stdcall "cl.h clEnqueueCopyBufferRect" clEnqueueCopyBufferRect ::
    CommandQueue -> CLMem -> CLMem -> Ptr SizeT -> Ptr SizeT -> Ptr SizeT -> SizeT -> SizeT -> SizeT -> SizeT ->  CLEventDriven

-- TODO: 5.2.3: Mapping buffer objects

--
-- 5.3: Image Objects
--

instance Storable CLImageFormat
    where alignment _ = #{alignment cl_image_format}
          sizeOf _ = #{size cl_image_format}
          peek ptr = 
              do chOrder <- #{peek cl_image_format, image_channel_order} ptr
                 chType <- #{peek cl_image_format, image_channel_data_type} ptr
                 return (CLImageFormat chOrder chType)
          poke ptr (CLImageFormat chOrder chType) =
              do #{poke cl_image_format, image_channel_order} ptr chOrder
                 #{poke cl_image_format, image_channel_data_type} ptr chType

foreign import stdcall "cl.h clCreateImage2D" clCreateImage2D :: Context -> CLMemFlags -> Ptr CLImageFormat -> SizeT -> SizeT -> SizeT -> Ptr a -> CLCtor CLMem
foreign import stdcall "cl.h clCreateImage3D" clCreateImage3D :: Context -> CLMemFlags -> Ptr CLImageFormat -> SizeT -> SizeT -> SizeT -> SizeT -> SizeT -> Ptr a -> CLCtor CLMem
foreign import stdcall "cl.h clGetSupportedImageFormats" clGetSupportedImageFormats :: Context -> CLMemFlags -> CLMemObjectType -> CLCountedArrayGetter(CLImageFormat)
foreign import stdcall "cl.h clEnqueueReadImage" clEnqueueReadImage :: CommandQueue -> CLMem -> Bool -> Ptr SizeT -> Ptr SizeT -> SizeT -> SizeT -> Ptr a -> CLEventDriven
foreign import stdcall "cl.h clEnqueueWriteImage" clEnqueueWriteImage :: CommandQueue -> CLMem -> Bool -> Ptr SizeT -> Ptr SizeT -> SizeT -> SizeT -> Ptr a -> CLEventDriven
foreign import stdcall "cl.h clEnqueueCopyImage" clEnqueueCopyImage :: CommandQueue -> CLMem -> CLMem -> Ptr SizeT -> Ptr SizeT -> Ptr SizeT -> CLEventDriven
foreign import stdcall "cl.h clEnqueueCopyImageToBuffer" clEnqueueCopyImageToBuffer :: CommandQueue -> CLMem -> CLMem -> Ptr SizeT -> Ptr SizeT -> SizeT -> CLEventDriven
foreign import stdcall "cl.h clEnqueueCopyBufferToImage" clEnqueueCopyBufferToImage :: CommandQueue -> CLMem -> CLMem -> SizeT -> Ptr SizeT -> Ptr SizeT -> CLEventDriven

-- TODO: Mapping image objects

foreign import stdcall "cl.h clGetImageInfo" clGetImageInfo :: CLMem -> CLImageInfo -> CLGetter

--
-- 5.4: Querying, Unmapping, Retaining and Releasing Memory Objects
--

foreign import stdcall "cl.h clRetainMemObject" clRetainMemObject :: CLMem -> IO CLInt
foreign import stdcall "cl.h clReleaseMemObject" clReleaseMemObject :: CLMem -> IO CLInt

-- TODO: clSetMemObjectDestructorCallback... again, how relevant in Haskell?
-- TODO: unmapping memory objects 

foreign import stdcall "cl.h clGetMemObjectInfo" clGetMemObjectInfo :: CLMem -> CLMemInfo -> CLGetter

--
-- 5.5: Sampler Objects
--

foreign import stdcall "cl.h clCreateSampler" clCreateSampler :: Context -> Bool -> CLAddressingMode -> CLFilterMode -> CLCtor Sampler
foreign import stdcall "cl.h clRetainSampler" clRetainSampler :: Sampler -> IO CLInt
foreign import stdcall "cl.h clReleaseSampler" clReleaseSampler :: Sampler -> IO CLInt
foreign import stdcall "cl.h clGetSamplerInfo" clGetSamplerInfo :: Sampler -> CLSamplerInfo -> CLGetter

--
-- 5.6 Program Objects
--
foreign import stdcall "cl.h clCreateProgramWithSource" clCreateProgramWithSource :: Context -> CLUInt -> Ptr CString -> Ptr SizeT -> CLCtor Program
foreign import stdcall "cl.h clCreateProgramWithBinary" clCreateProgramWithBinary :: Context -> CLUInt -> Ptr Device -> Ptr SizeT -> Ptr (Ptr CUChar) -> Ptr CLInt -> CLCtor Program
foreign import stdcall "cl.h clRetainProgram" clRetainProgram :: Program -> IO CLInt
foreign import stdcall "cl.h clReleaseProgram" clReleaseProgram :: Program -> IO CLInt
foreign import stdcall "wrapper" makeBuildProgramCallback :: (Program -> Ptr a -> IO ()) -> IO (FunPtr (Program -> Ptr a -> IO ()))
foreign import stdcall "cl.h clBuildProgram" clBuildProgram :: Program -> CLUInt -> Ptr Device -> CString -> FunPtr (Program -> Ptr a -> IO ()) -> Ptr a -> IO CLInt
foreign import stdcall "cl.h clUnloadCompiler" clUnloadCompiler :: IO ()
foreign import stdcall "cl.h clGetProgramInfo" clGetProgramInfo :: Program -> CLProgramInfo -> CLGetter
foreign import stdcall "cl.h clGetProgramBuildInfo" clGetProgramBuildInfo :: Program -> Device -> CLProgramBuildInfo -> CLGetter

--
-- 5.7: Kernel Objects
--

foreign import stdcall "cl.h clCreateKernel" clCreateKernel :: Program -> CString -> CLCtor Kernel
foreign import stdcall "cl.h clCreateKernelsInProgram" clCreateKernelsInProgram :: Program -> CLCountedArrayGetter(Kernel)
foreign import stdcall "cl.h clRetainKernel" clRetainKernel :: Kernel -> IO CLInt
foreign import stdcall "cl.h clReleaseKernel" clReleaseKernel :: Kernel -> IO CLInt
foreign import stdcall "cl.h clSetKernelArg" clSetKernelArg :: Kernel -> CLUInt -> SizeT -> Ptr a -> IO CLInt
foreign import stdcall "cl.h clGetKernelInfo" clGetKernelInfo :: Kernel -> CLKernelInfo -> CLGetter
foreign import stdcall "cl.h clGetKernelWorkGroupInfo" clGetKernelWorkGroupInfo :: Kernel -> Device -> CLKernelWorkGroupInfo -> CLGetter

--
-- 5.8: Executing Kernels
--

foreign import stdcall "cl.h clEnqueueNDRangeKernel" clEnqueueNDRangeKernel :: CommandQueue -> Kernel -> CLUInt -> Ptr SizeT -> Ptr SizeT -> Ptr SizeT -> CLEventDriven
foreign import stdcall "cl.h clEnqueueTask" clEnqueueTask :: CommandQueue -> Kernel -> CLEventDriven
foreign import stdcall "wrapper" makeNativeKernel :: IO () -> IO (FunPtr (IO ()))
foreign import stdcall "cl.h clEnqueueNativeKernel" clEnqueueNativeKernel :: CommandQueue -> FunPtr (IO ()) -> Ptr a -> SizeT -> CLUInt -> Ptr CLMem -> Ptr (Ptr a) -> CLEventDriven

--
-- 5.9: Event Objects
--

foreign import stdcall "cl.h clCreateUserEvent" clCreateUserEvent :: Context -> CLCtor Event
foreign import stdcall "cl.h clSetUserEventStatus" clSetUserEventStatus :: Event -> CLInt -> IO CLInt
foreign import stdcall "cl.h clWaitForEvents" clWaitForEvents :: CLUInt -> Ptr Event -> IO CLInt

foreign import stdcall "cl.h clGetEventInfo" clGetEventInfo :: Event -> CLEventInfo -> CLGetter

foreign import stdcall "cl.h clSetEventCallback" clSetEventCallback :: Event -> CLInt -> FunPtr (CLEventCallback a) -> Ptr a -> IO CLInt
foreign import stdcall "wrapper" makeEventCallback :: CLEventCallback a -> IO (FunPtr (CLEventCallback a))
foreign import stdcall "cl.h clRetainEvent" clRetainEvent :: Event -> IO CLInt
foreign import stdcall "cl.h clReleaseEvent" clReleaseEvent :: Event -> IO CLInt

--
-- 5.10: Markers, Barriers, and Waiting for Events
--

foreign import stdcall "cl.h clEnqueueMarker" clEnqueueMarker :: CommandQueue -> Ptr Event -> IO CLInt 
foreign import stdcall "cl.h clEnqueueBarrier" clEnqueueBarrier :: CommandQueue -> IO CLInt
foreign import stdcall "cl.h clEnqueueWaitForEvents" clEnqueueWaitForEvents :: CommandQueue -> CLUInt -> Ptr Event -> IO CLInt

--
-- 5.12: Profiling Operations on Memory Objects and Kernels
--

foreign import stdcall "cl.h clGetEventProfilingInfo" clGetEventProfilingInfo :: Event -> CLProfilingInfo  -> CLGetter

-- 
-- 5.13: Flush and Finish
--

foreign import stdcall "cl.h clFlush" clFlush :: CommandQueue -> IO CLInt
foreign import stdcall "cl.h clFinish" clFinish :: CommandQueue -> IO CLInt

-- Here be dragons
