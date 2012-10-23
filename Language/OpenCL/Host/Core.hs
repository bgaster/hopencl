{-# LANGUAGE OverlappingInstances, ScopedTypeVariables, NoMonomorphismRestriction, GADTs, TypeSynonymInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Language.OpenCL.Host.Core where

import Language.OpenCL.Host.Constants
import Language.OpenCL.Host.FFI

import Control.Exception
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Array.Storable hiding (newArray)
import Data.Typeable
import Foreign
import Foreign.C
import Prelude hiding (catch)

data CLError = DeviceNotFound | DeviceNotAvailable | CompilerNotAvailable | MemObjectAllocationFailure 
             | OutOfResources | OutOfHostMemory | ProfilingInfoNotAvailable | MemCopyOverlap 
             | ImageFormatMismatch | ImageFormatNotSupported | BuildProgramFailure | MapFailure 
             | MisalignedSubBufferOffset | ExecStatusErrorForEventsInWaitList | InvalidValue 
             | InvalidDeviceType | InvalidPlatform | InvalidDevice | InvalidContext | InvalidQueueProperties 
             | InvalidCommandQueue | InvalidHostPtr | InvalidMemObject | InvalidImageFormatDescriptor 
             | InvalidImageSize | InvalidSampler | InvalidBinary | InvalidBuildOptions | InvalidProgram 
             | InvalidProgramExecutable | InvalidKernelName | InvalidKernelDefinition | InvalidKernel 
             | InvalidArgIndex |  InvalidArgValue | InvalidArgSize | InvalidKernelArgs | InvalidWorkDimension 
             | InvalidWorkGroupSize | InvalidWorkItemSize | InvalidGlobalOffset | InvalidEventWaitList 
             | InvalidEvent | InvalidOperation | InvalidGLObject | InvalidBufferSize | InvalidMipLevel 
             | InvalidGlobalWorkSize deriving (Bounded, Enum, Eq, Ord, Typeable)

instance Const CLError CLInt
    where value DeviceNotFound                     = -1
          value DeviceNotAvailable                 = -2
          value CompilerNotAvailable               = -3
          value MemObjectAllocationFailure         = -4
          value OutOfResources                     = -5
          value OutOfHostMemory                    = -6
          value ProfilingInfoNotAvailable          = -7
          value MemCopyOverlap                     = -8
          value ImageFormatMismatch                = -9
          value ImageFormatNotSupported            = -10
          value BuildProgramFailure                = -11
          value MapFailure                         = -12
          value MisalignedSubBufferOffset          = -13
          value ExecStatusErrorForEventsInWaitList = -14
          value InvalidValue                       = -30
          value InvalidDeviceType                  = -31
          value InvalidPlatform                    = -32
          value InvalidDevice                      = -33
          value InvalidContext                     = -34
          value InvalidQueueProperties             = -35
          value InvalidCommandQueue                = -36
          value InvalidHostPtr                     = -37
          value InvalidMemObject                   = -38
          value InvalidImageFormatDescriptor       = -39
          value InvalidImageSize                   = -40
          value InvalidSampler                     = -41
          value InvalidBinary                      = -42
          value InvalidBuildOptions                = -43
          value InvalidProgram                     = -44
          value InvalidProgramExecutable           = -45
          value InvalidKernelName                  = -46
          value InvalidKernelDefinition            = -47
          value InvalidKernel                      = -48
          value InvalidArgIndex                    = -49
          value InvalidArgValue                    = -50
          value InvalidArgSize                     = -51
          value InvalidKernelArgs                  = -52
          value InvalidWorkDimension               = -53
          value InvalidWorkGroupSize               = -54
          value InvalidWorkItemSize                = -55
          value InvalidGlobalOffset                = -56
          value InvalidEventWaitList               = -57
          value InvalidEvent                       = -58
          value InvalidOperation                   = -59
          value InvalidGLObject                    = -60
          value InvalidBufferSize                  = -61
          value InvalidMipLevel                    = -62
          value InvalidGlobalWorkSize              = -63

instance Show CLError 
    where show DeviceNotFound                     = "Device not found"
          show DeviceNotAvailable                 = "Device not available"
          show CompilerNotAvailable               = "Compiler not available"
          show MemObjectAllocationFailure         = "Memory object allocation failure"
          show OutOfResources                     = "Out of resources"
          show OutOfHostMemory                    = "Out of host memory"
          show ProfilingInfoNotAvailable          = "Profiling information not available"
          show MemCopyOverlap                     = "Memory copy overlap"
          show ImageFormatMismatch                = "Image format mismatch"
          show ImageFormatNotSupported            = "Image format not supported"
          show BuildProgramFailure                = "Program build failure"
          show MapFailure                         = "Map failure"
          show MisalignedSubBufferOffset          = "Misaligned subbuffer offset"
          show ExecStatusErrorForEventsInWaitList = "Execution status error for events in wait list"
          show InvalidValue                       = "Invalid value"
          show InvalidDeviceType                  = "Invalid device type"
          show InvalidPlatform                    = "Invalid platform"
          show InvalidDevice                      = "Invalid device"
          show InvalidContext                     = "Invalid context"
          show InvalidQueueProperties             = "Invalid queue properties"
          show InvalidCommandQueue                = "Invalid command queue"
          show InvalidHostPtr                     = "Invalid host pointer"
          show InvalidMemObject                   = "Invalid memory object"
          show InvalidImageFormatDescriptor       = "Invalid image format descriptor"
          show InvalidImageSize                   = "Invalid image size"
          show InvalidSampler                     = "Invalid sampler"
          show InvalidBinary                      = "Invalid binary"
          show InvalidBuildOptions                = "Invalid build options"
          show InvalidProgram                     = "Invalid program"            
          show InvalidProgramExecutable           = "Invalid program executable" 
          show InvalidKernelName                  = "Invalid kernel name"          
          show InvalidKernelDefinition            = "Invalid kernel definition"
          show InvalidKernel                      = "Invalid kernel"
          show InvalidArgIndex                    = "Invalid argument index"
          show InvalidArgValue                    = "Invalid argument value"
          show InvalidArgSize                     = "Invalid argument size"
          show InvalidKernelArgs                  = "Invalid kernel arguments"
          show InvalidWorkDimension               = "Invalid work dimension"
          show InvalidWorkGroupSize               = "Invalid work group size"
          show InvalidWorkItemSize                = "Invalid work item size"
          show InvalidGlobalOffset                = "Invalid global offset"
          show InvalidEventWaitList               = "Invalid event wait list"
          show InvalidEvent                       = "Invalid event"
          show InvalidOperation                   = "Invalid operation"
          show InvalidGLObject                    = "Invalid GL object"
          show InvalidBufferSize                  = "Invalid buffer size"
          show InvalidMipLevel                    = "Invalid mip level"
          show InvalidGlobalWorkSize              = "Invalid global work size"

data Error = CLError CLError [String] | UserError String [String] deriving (Eq, Typeable)

appendLocation :: String -> Error -> Error
appendLocation s (CLError err location) = CLError err (location ++ [s])
appendLocation s (UserError err location) = UserError err (location ++ [s])

appendingLocation :: String -> IO t -> IO t
appendingLocation location comp = comp `catch` appender
    where appender :: Error -> IO t
          appender err = throw $ appendLocation location err

instance Show Error
    where show (CLError c location)   = "OpenCL error: " ++ show c ++ concatMap ("\n    at " ++) location
          show (UserError s location) = "User error: " ++ s ++ concatMap ("\n    at " ++) location

instance Exception Error

errorFromErrorCode :: CLInt -> Error
errorFromErrorCode errc =
    case lookup errc [(value c, c) | c <- [minBound..maxBound]] of
      Nothing  -> UserError ("Uninterpreted error code " ++ show errc) []
      Just err -> CLError err []
                  
clFail errc location = throw (appendLocation location $ errorFromErrorCode errc)
userFail s location = throw (UserError s [location])

handleCLError :: String -> IO CLInt -> IO ()
handleCLError location cl = 
    do r <- cl
       if r /= 0 then clFail r location else return ()

handleCLCreationError :: String -> (Ptr CLInt -> IO t) -> IO t
handleCLCreationError location ctor = 
    alloca $ \ errPtr -> 
        do v <- ctor errPtr
           r <- peek errPtr
           if r /= 0 then clFail r location else return v

handleCLEventDriven :: String -> (CLUInt -> Ptr Event -> Ptr Event -> IO CLInt) -> [Event] -> IO Event
handleCLEventDriven location cl events = 
    if null events
    then alloca $ \ evPtr -> 
            do handleCLError location $ cl 0 nullPtr evPtr
               peek evPtr
    else let nEvents = length events 
         in withArray events $ \ evArray -> alloca $ \ evPtr -> 
                do handleCLError location $ cl (fromIntegral nEvents) evArray evPtr
                   peek evPtr

-- Generic getters

getStorable :: forall t. Storable t => (SizeT -> Ptr t -> Ptr SizeT -> IO CLInt) -> IO t
getStorable getter = alloca $ \ptr -> 
                     do handleCLError "getStorable" $ getter (fromIntegral (sizeOf (undefined :: t))) ptr nullPtr
                        peek ptr
                           
getBitSet :: forall t u. (Const t u, Integral u, Bits u, Storable u) => (SizeT -> Ptr u -> Ptr SizeT -> IO CLInt) -> IO (BitSet t)
getBitSet getter = alloca $ \ptr ->
                   do handleCLError "getBitSet" $ getter (fromIntegral (sizeOf (undefined :: u))) ptr nullPtr
                      BitSet `fmap` peek ptr

getConstEnum :: forall t u. (Const t u, Bounded t, Enum t, Eq u, Storable u, Show u) => (SizeT -> Ptr u -> Ptr SizeT -> IO CLInt) -> IO t
getConstEnum getter = alloca $ \ptr ->
                      do handleCLError "getConstEnum" $ getter (fromIntegral (sizeOf (undefined :: u))) ptr nullPtr
                         v <- peek ptr
                         case filter ((v ==) . snd) [(c, value c) | c <- [minBound .. maxBound]] of
                           ((c, _):_) -> return c
                           _          -> error $ "Unable to interpret " ++ show v

genericGetArray :: forall t u. Storable u => (Int -> Ptr u -> IO [t]) -> (SizeT -> Ptr u -> Ptr SizeT -> IO CLInt) -> IO [t]
genericGetArray peeker getter = 
    do size <- alloca $ \ sizePtr -> 
           do handleCLError "genericGetArray" $ getter 0 nullPtr sizePtr
              peek sizePtr
       let nElements = fromIntegral size `div` sizeOf (undefined :: u)
       allocaArray nElements $ \ ptr -> 
           do handleCLError "genericGetArray" $ getter size ptr nullPtr
              peeker nElements ptr

-- FIXME: this doesn't work.  Output of 'compile' example, for instance
getString :: (SizeT -> Ptr CChar -> Ptr SizeT -> IO CLInt) -> IO [Char]
getString = appendingLocation "getString" . genericGetArray (const peekCString)

getArray :: forall t. Storable t => (SizeT -> Ptr t -> Ptr SizeT -> IO CLInt) -> IO [t]
getArray = appendingLocation "getArray" . genericGetArray peekArray

getCountedArray :: forall t. Storable t => (CLUInt -> Ptr t -> Ptr CLUInt -> IO CLInt) -> IO [t]
getCountedArray getter = 
    do count <- alloca $ \ countPtr -> 
           do handleCLError "getCountedArray" $ getter 0 nullPtr countPtr
              peek countPtr
       let count' = fromIntegral count
       allocaArray count' $ \ ptr -> 
           do handleCLError "getCountedArray" $ getter count ptr nullPtr
              peekArray count' ptr

--
-- Platform stuff
--

getPlatforms :: IO [Platform]
getPlatforms = appendingLocation "getPlatforms" $ 
               getCountedArray clGetPlatformIDs

getPlatformInfo :: Platform -> PlatformInfo u -> IO u
getPlatformInfo platform plInf = 
    case plInf of
      PlatformProfile    -> get
      PlatformVersion    -> get
      PlatformName       -> get
      PlatformVendor     -> get
      PlatformExtensions -> get
    where get = appendingLocation "getPlatformInfo" $ getString (clGetPlatformInfo platform (value plInf))

--
-- Device stuff
--

getDevices :: Platform -> BitSet DeviceType -> IO [Device]
getDevices platform devTypes = appendingLocation "getDevices" $ 
                               getCountedArray (clGetDeviceIDs platform (valueFrom devTypes))

getDeviceInfo :: Device -> DeviceInfo t -> IO t 
getDeviceInfo device devInfo = 
    case devInfo of
      DeviceType                     -> bitSetHelper
      DeviceVendorID                 -> stoHelper
      DeviceMaxComputeUnits          -> stoHelper
      DeviceMaxWorkItemDimensions    -> stoHelper
      DeviceMaxWorkItemSizes         -> arrayHelper

      DeviceMaxWorkGroupSize         -> stoHelper
      DevicePreferredVectorWidth _   -> stoHelper
      DeviceNativeVectorWidth _      -> unsupported
      DeviceMaxClockFrequency        -> stoHelper
      DeviceAddressBits              -> stoHelper

      DeviceMaxMemAllocSize          -> stoHelper

      DeviceImageSupport             -> stoHelper
      DeviceMaxReadImageArgs         -> stoHelper
      DeviceMaxWriteImageArgs        -> stoHelper
      DeviceImage2DMaxWidth          -> stoHelper
      DeviceImage2DMaxHeight         -> stoHelper
      DeviceImage3DMaxWidth          -> stoHelper
      DeviceImage3DMaxHeight         -> stoHelper
      DeviceImage3DMaxDepth          -> stoHelper
      DeviceMaxSamplers              -> stoHelper

      DeviceMaxParameterSize         -> stoHelper

      DeviceMemBaseAddrAlign         -> stoHelper
      DeviceMinDataTypeAlignSize     -> stoHelper

      DeviceSingleFPConfig           -> bitSetHelper

      DeviceGlobalMemCacheType       -> constEnumHelper
      DeviceGlobalMemCachelineSize   -> stoHelper
      DeviceGlobalMemCacheSize       -> stoHelper
      DeviceGlobalMemSize            -> stoHelper

      DeviceMaxConstantBufferSize    -> stoHelper
      DeviceMaxConstantArgs          -> stoHelper

      DeviceLocalMemType             -> constEnumHelper
      DeviceLocalMemSize             -> stoHelper
      DeviceErrorCorrectionSupport   -> stoHelper

      DeviceHostUnifiedMemory        -> stoHelper

      DeviceProfilingTimerResolution -> stoHelper

      DeviceEndianLittle             -> stoHelper

      DeviceAvailable                -> stoHelper
      DeviceCompilerAvailable        -> stoHelper

      DeviceExecutionCapabilities    -> bitSetHelper

      DeviceQueueProperties          -> bitSetHelper

      DevicePlatform                 -> stoHelper

      DeviceName                     -> stringHelper      
      DeviceVendor                   -> stringHelper      
      DriverVersion                  -> stringHelper      
      DeviceProfile                  -> stringHelper      
      DeviceVersion                  -> stringHelper      
      DeviceOpenCLCVersion           -> stringHelper      
      DeviceExtensions               -> stringHelper      
      
    where getter          :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
          stoHelper       :: forall t. Storable t => IO t
          bitSetHelper    :: forall t u. (Const t u, Integral u, Bits u, Storable u) => IO (BitSet t)
          arrayHelper     :: forall t. Storable t => IO [t]
          constEnumHelper :: forall t u. (Enum t, Bounded t, Const t u, Eq u, Storable u, Show u) => IO t

          getter          = clGetDeviceInfo device (value devInfo)          
          stoHelper       = getStorable getter
          bitSetHelper    = getBitSet getter
          arrayHelper     = getArray getter
          stringHelper    = getString getter
          constEnumHelper = getConstEnum getter

--
-- Contexts
--

createContext :: ContextProperties -> [Device] -> IO Context
createContext properties devices = 
    withContextProperties properties $ \ contextProperties ->
    withArray devices $ \ deviceArray ->
        handleCLCreationError "createContext" (clCreateContext contextProperties (fromIntegral (length devices)) deviceArray nullFunPtr nullPtr)
                       
createContextFromType :: ContextProperties -> BitSet DeviceType -> IO Context
createContextFromType properties devTypes = 
    withContextProperties properties $ \ contextProperties ->
        handleCLCreationError "createContextFromType" (clCreateContextFromType contextProperties (valueFrom devTypes) nullFunPtr nullPtr)
    
retainContext :: Context -> IO ()
retainContext = handleCLError "retainContext" . clRetainContext

releaseContext :: Context -> IO ()
releaseContext = handleCLError "retainContext" . clReleaseContext

getContextInfo :: Context -> ContextInfo t -> IO t
getContextInfo context ctxInfo =
    case ctxInfo of
      ContextReferenceCount -> stoHelper
      ContextNumDevices     -> stoHelper
      ContextDevices        -> arrHelper
      ContextProperties     -> arrHelper >>= return . CPs
    where getter          :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
          stoHelper       :: forall t. Storable t => IO t
          arrHelper       :: forall t. Storable t => IO [t]
          
          getter = clGetContextInfo context (value ctxInfo)
          stoHelper = getStorable getter
          arrHelper = getArray getter

-- 
-- Command Queues
--

createCommandQueue :: Context -> Device -> BitSet CommandQueueProperty -> IO CommandQueue
createCommandQueue context device properties = 
    handleCLCreationError "createCommandQueue" $ clCreateCommandQueue context device (valueFrom properties)

retainCommandQueue, releaseCommandQueue :: CommandQueue -> IO ()
retainCommandQueue = handleCLError "retainCommandQueue" . clRetainCommandQueue
releaseCommandQueue = handleCLError "releaseCommandQueue" . clReleaseCommandQueue

getCommandQueueInfo :: CommandQueue -> CommandQueueInfo t -> IO t
getCommandQueueInfo queue qInfo =
    case qInfo of
      QueueContext        -> stoHelper 
      QueueDevice         -> stoHelper 
      QueueReferenceCount -> stoHelper 
      QueueProperties     -> bitSetHelper
    where getter          :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
          stoHelper       :: forall t. Storable t => IO t
          bitSetHelper    :: forall t u. (Const t u, Integral u, Bits u, Storable u) => IO (BitSet t)

          getter = clGetCommandQueueInfo queue (value qInfo)
          stoHelper = getStorable getter
          bitSetHelper = getBitSet getter

--
-- Buffer Objects
--

-- This API is a work-in-progress, let's say.

newtype Buffer t = Buffer { unBuffer :: CLMem }
instance Storable (Buffer t)
    where sizeOf _ = sizeOf (undefined :: CLMem)
          alignment _ = alignment (undefined :: CLMem)
          poke ptr (Buffer mem) = poke (castPtr ptr) mem
          peek ptr = error "Can't peek buffers"

createBuffer :: forall t. Storable t => Context -> BitSet MemFlag -> Int -> IO (Buffer t)
createBuffer context flags elemCount = 
    Buffer `fmap` (handleCLCreationError "createBuffer" $ clCreateBuffer context (valueFrom flags) bytesNeeded nullPtr)
    where bytesNeeded = fromIntegral (elemCount * sizeOf (undefined :: t))

createSubBuffer :: forall t. Storable t => Buffer t -> BitSet MemFlag -> Int -> Int -> IO (Buffer t)
createSubBuffer (Buffer mem) flags origin elemCount =
    do alloca $ \ regionPtr -> 
           do poke regionPtr (CLBufferRegion byteOrigin bytesNeeded)
              Buffer `fmap` (handleCLCreationError "createSubBuffer" $ clCreateSubBuffer mem (valueFrom flags) (value CreateTypeRegion) regionPtr)
    where byteOrigin = fromIntegral $ origin * sizeOf (undefined :: t)
          bytesNeeded = fromIntegral $ elemCount * sizeOf (undefined :: t)


-- No point returning the event, as this is a blocking read
enqueueBlockingReadBuffer :: forall t. Storable t => CommandQueue -> Buffer t -> Int -> Int -> [Event] -> IO [t]
enqueueBlockingReadBuffer queue (Buffer mem) origin elemCount waitEvents =
    do allocaArray elemCount $ \ arrayPtr -> 
           do handleCLEventDriven "enqueueBlockingReadBuffer" (clEnqueueReadBuffer queue mem True byteOrigin byteRange arrayPtr) waitEvents
              peekArray elemCount arrayPtr
    where byteOrigin = fromIntegral $ origin * sizeOf (undefined :: t)
          byteRange = fromIntegral $ elemCount * sizeOf (undefined :: t)

enqueueBlockingReadBufferSA :: forall t. Storable t => CommandQueue -> Buffer t -> Int -> Int -> [Event] -> IO (StorableArray Int t)
enqueueBlockingReadBufferSA queue (Buffer mem) origin elemCount waitEvents =
    do ar <- newArray_ (origin, elemCount - 1)
       withStorableArray ar $ \ arrayPtr ->
           handleCLEventDriven "enqueueBlockingReadBufferSA" (clEnqueueReadBuffer queue mem True byteOrigin byteRange arrayPtr) waitEvents
       return ar
    where byteOrigin = fromIntegral $ origin * sizeOf (undefined :: t)
          byteRange = fromIntegral $ elemCount * sizeOf (undefined :: t)

-- No idea if this is the right function or not
enqueueNonBlockingReadBuffer :: forall t. Storable t => CommandQueue -> Buffer t -> Int -> Int -> ([t] -> IO ()) -> [Event] -> IO Event
enqueueNonBlockingReadBuffer queue (Buffer mem) origin elemCount continuation waitEvents =
    do arrayPtr <- mallocArray elemCount
       ev <- handleCLEventDriven "enqueueNonBlockingReadBuffer" (clEnqueueReadBuffer queue mem False byteOrigin byteRange arrayPtr) waitEvents
       setDatalessEventCallback ev (\_ _ _ -> do r <- peekArray elemCount arrayPtr
                                                 free arrayPtr
                                                 continuation r)
       return ev
    where byteOrigin = fromIntegral $ origin * sizeOf (undefined :: t)
          byteRange = fromIntegral $ elemCount * sizeOf (undefined :: t)


-- No point returning the event--see above
enqueueBlockingWriteBuffer :: forall t. Storable t => CommandQueue -> Buffer t -> Int -> [t] -> [Event] -> IO () 
enqueueBlockingWriteBuffer queue (Buffer mem) origin elems waitEvents =
    do allocaArray (length elems) $ \ arrayPtr ->
           do pokeArray arrayPtr elems
              handleCLEventDriven "enqueueBlockingWriteBuffer"(clEnqueueWriteBuffer queue mem True byteOrigin byteRange arrayPtr) waitEvents
              return ()
    where size = sizeOf (undefined :: t)
          byteOrigin = fromIntegral (origin * size)
          byteRange = fromIntegral (length elems * size)

enqueueBlockingWriteBufferSA :: forall t. Storable t => CommandQueue -> Buffer t -> Int -> StorableArray Int t -> [Event] -> IO Event
enqueueBlockingWriteBufferSA queue (Buffer mem) origin elems waitEvents = 
    do (low, high) <- getBounds elems
       withStorableArray elems $ \ arrayPtr ->
           handleCLEventDriven "enqueueBlockingWriteBufferSA" (clEnqueueWriteBuffer queue mem True byteOrigin (byteRange (high - low + 1)) arrayPtr) waitEvents
    where size = sizeOf (undefined :: t)
          byteOrigin = fromIntegral (origin * size)          
          byteRange count = fromIntegral (count * size)

enqueueNonBlockingWriteBuffer :: forall t. Storable t => CommandQueue -> Buffer t -> Int -> [t] -> [Event] -> IO Event
enqueueNonBlockingWriteBuffer queue (Buffer mem) origin elems waitEvents =
    do arrayPtr <- newArray elems
       ev <- handleCLEventDriven "enqueueNonBlockingWriteBuffer" (clEnqueueWriteBuffer queue mem False byteOrigin byteRange arrayPtr) waitEvents
       setDatalessEventCallback ev (\_ _ _ -> free arrayPtr)
       return ev
    where size = sizeOf (undefined :: t)
          byteOrigin = fromIntegral (origin * size)
          byteRange = fromIntegral (length elems * size)

-- TODO: enqueueReadBufferRect and enqueueWriteBufferRect, as I don't understand them

enqueueCopyBuffer :: forall t. Storable t => CommandQueue -> Buffer t -> Buffer t -> Int -> Int -> Int -> [Event] -> IO Event
enqueueCopyBuffer queue (Buffer mem0) (Buffer mem1) origin0 origin1 nElems =
    handleCLEventDriven "enqueueCopyBuffer" (clEnqueueCopyBuffer queue mem0 mem1 byteOrigin0 byteOrigin1 byteRange)
    where size = sizeOf (undefined :: t)
          byteOrigin0 = fromIntegral $ origin0 * size
          byteOrigin1 = fromIntegral $ origin1 * size
          byteRange = fromIntegral $ nElems * size


-- TODO: enqueueCopyBufferRect: see above whining about rects

retainBuffer, releaseBuffer :: Buffer t -> IO ()
retainBuffer (Buffer mem) = handleCLError "retainBuffer" (clRetainMemObject mem)
releaseBuffer (Buffer mem) = handleCLError "releaseBuffer" (clReleaseMemObject mem)

getMemObjectInfo :: CLMem -> MemInfo u -> IO u
getMemObjectInfo mem memInfo =
    case memInfo of
      MemType                -> constEnumHelper
      MemFlags               -> bitSetHelper
      MemSize                -> stoHelper
      MemHostPtr             -> stoHelper
      MemMapCount            -> stoHelper
      MemReferenceCount      -> stoHelper
      MemContext             -> stoHelper
      MemAssociatedMemObject -> stoHelper
      MemOffset              -> stoHelper
    where getter          :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
          stoHelper       :: forall t. Storable t => IO t
          bitSetHelper    :: forall t u. (Const t u, Integral u, Bits u, Storable u) => IO (BitSet t)
          constEnumHelper :: forall t u. (Enum t, Bounded t, Const t u, Eq u, Storable u, Show u) => IO t
                    
          getter = clGetMemObjectInfo mem (value memInfo)
          stoHelper = getStorable getter
          bitSetHelper = getBitSet getter
          constEnumHelper = getConstEnum getter

getBufferInfo (Buffer mem) = getMemObjectInfo mem

--
-- Images
--

data Image t = Image { imageMemObject :: CLMem
                     , imageRowPitch :: SizeT
                     , imageSlicePitch :: SizeT }

create2DImage :: forall t. Storable t => Context -> BitSet MemFlag -> CLImageFormat -> Int -> Int -> IO (Image t)
create2DImage context flags format width height = 
    alloca $ \ formatPtr -> 
        do poke formatPtr format
           mem <- handleCLCreationError "create2DImage" $ clCreateImage2D context (valueFrom flags) formatPtr byteWidth byteHeight 0 nullPtr
           return (Image mem byteWidth 0)
    where size = sizeOf (undefined :: t)
          byteWidth = fromIntegral $ size * width
          byteHeight = fromIntegral $ size * height

create3DImage :: forall t. Storable t => Context -> BitSet MemFlag -> CLImageFormat -> Int -> Int -> Int -> IO (Image t)
create3DImage context flags format width height depth = 
    alloca $ \ formatPtr -> 
        do poke formatPtr format
           mem <- handleCLCreationError "create3DImage" $ clCreateImage3D context (valueFrom flags) formatPtr byteWidth byteHeight byteDepth 0 0 nullPtr
           return (Image mem byteWidth (byteWidth * byteHeight))
    where size       = sizeOf (undefined :: t)
          byteWidth  = fromIntegral $ size * width
          byteHeight = fromIntegral $ size * height
          byteDepth  = fromIntegral $ size * depth

getSupportedImageFormats :: Context -> BitSet MemFlag -> MemObjectType -> IO [CLImageFormat]
getSupportedImageFormats context flags objType = getCountedArray $ clGetSupportedImageFormats context (valueFrom flags) (value objType)

type Point = (Int, Int, Int)

allocaTriple :: Storable t => (t,t,t) -> (Ptr t -> IO r) -> IO r
allocaTriple (x,y,z) c = 
    allocaArray 3 $ \ triplePtr ->
        do pokeArray triplePtr [x,y,z]
           c triplePtr

fit (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)

-- TODO: [t] seems like absolutely the wrong return type, but I'm not sure what would be better.
-- Adjust this when I have actual applications.
enqueueBlockingReadImage :: forall t. Storable t => CommandQueue -> Image t -> Point -> Point -> [Event] -> IO [t]
enqueueBlockingReadImage queue (Image mem rowPitch slicePitch) origin range@(dx, dy, dz) waitEvents = 
    allocaTriple (fit origin) $ \ originPtr ->
    allocaTriple (fit range) $ \ rangePtr ->
    allocaArray resultCount $ \ arrayPtr ->
        do handleCLEventDriven "enqueueBlockingReadImage" (clEnqueueReadImage queue mem True originPtr rangePtr rowPitch slicePitch arrayPtr) waitEvents
           peekArray resultCount arrayPtr
    where resultCount = dx * dy * dz

enqueueNonBlockingReadImage :: forall t. Storable t => CommandQueue -> Image t -> Point -> Point -> ([t] -> IO ()) -> [Event] -> IO Event
enqueueNonBlockingReadImage queue (Image mem rowPitch slicePitch) origin range@(dx, dy, dz) continuation waitEvents = 
    allocaTriple (fit origin) $ \ originPtr ->
    allocaTriple (fit range) $ \ rangePtr ->
        do arrayPtr <- mallocArray resultCount
           ev <- handleCLEventDriven "enqueueNonBlockingReadImage" (clEnqueueReadImage queue mem False originPtr rangePtr rowPitch slicePitch arrayPtr) waitEvents
           setDatalessEventCallback ev (\_ _ _ -> do r <- peekArray resultCount arrayPtr
                                                     free arrayPtr
                                                     continuation r)
           return ev                                                    
    where resultCount = dx * dy * dz
    

-- Again, [t] is awful here
enqueueBlockingWriteImage :: forall t. Storable t => CommandQueue -> Image t -> Point -> Point -> [t] -> [Event] -> IO ()
enqueueBlockingWriteImage queue (Image mem rowPitch slicePitch) origin range@(dx, dy, dz) elems waitEvents 
    | length elems < resultCount = userFail "Not enough elements passed to enqueueBlockingWriteImage" []
    | otherwise = 
        allocaTriple (fit origin) $ \ originPtr ->
        allocaTriple (fit range) $ \ rangePtr ->
        allocaArray resultCount $ \ arrayPtr -> 
            do pokeArray arrayPtr (take resultCount elems)
               handleCLEventDriven "enqueueBlockingWriteImage" (clEnqueueWriteImage queue mem True originPtr rangePtr rowPitch slicePitch arrayPtr) waitEvents
               return ()
    where resultCount = dx * dy * dz

enqueueNonblockingWriteImage :: forall t. Storable t => CommandQueue -> Image t -> Point -> Point -> [t] -> [Event] -> IO Event
enqueueNonblockingWriteImage queue (Image mem rowPitch slicePitch) origin range@(dx, dy, dz) elems waitEvents 
    | length elems < resultCount = userFail "Not enough elements passed to enqueueBlockingWriteImage" []
    | otherwise = 
        allocaTriple (fit origin) $ \ originPtr ->
        allocaTriple (fit range) $ \ rangePtr ->
            do arrayPtr <- newArray (take resultCount elems)
               ev <- handleCLEventDriven "enqueueNonblockingWriteImage" (clEnqueueWriteImage queue mem True originPtr rangePtr rowPitch slicePitch arrayPtr) waitEvents
               setDatalessEventCallback ev (\_ _ _ -> free arrayPtr)
               return ev
    where resultCount = dx * dy * dz

enqueueCopyImage :: CommandQueue -> Image t -> Image t -> Point -> Point -> Point -> [Event] -> IO Event
enqueueCopyImage queue (Image mem0 _ _) (Image mem1 _ _) origin0 origin1 range waitEvents =
    allocaTriple (fit origin0) $ \ origin0Ptr ->
    allocaTriple (fit origin1) $ \ origin1Ptr ->
    allocaTriple (fit range) $ \ rangePtr ->
        handleCLEventDriven "enqueueCopyImage" (clEnqueueCopyImage queue mem0 mem1 origin0Ptr origin1Ptr rangePtr) waitEvents

enqueueCopyImageToBuffer :: CommandQueue -> Image t -> Buffer t -> Point -> Point -> Int -> [Event] -> IO Event
enqueueCopyImageToBuffer queue (Image mem0 _ _) (Buffer mem1) origin0 range origin1 waitEvents =
    allocaTriple (fit origin0) $ \ origin0Ptr -> 
    allocaTriple (fit range) $ \ rangePtr -> 
        handleCLEventDriven "enqueueCopyImageToBuffer" (clEnqueueCopyImageToBuffer queue mem0 mem1 origin0Ptr rangePtr (fromIntegral origin1)) waitEvents

enqueueCopyBufferToImage :: CommandQueue -> Buffer t -> Image t -> Int -> Point -> Point -> [Event] -> IO Event
enqueueCopyBufferToImage queue (Buffer mem0) (Image mem1 _ _) origin0 origin1 range waitEvents = 
    allocaTriple (fit origin1) $ \ origin1Ptr ->
    allocaTriple (fit range) $ \ rangePtr ->
        handleCLEventDriven "enqueueCopyBufferToImage" (clEnqueueCopyBufferToImage queue mem0 mem1 (fromIntegral origin0) origin1Ptr rangePtr) waitEvents

getImageMemObjectInfo :: Image t -> MemInfo u -> IO u
getImageMemObjectInfo (Image img _ _) = getMemObjectInfo img

getImageInfo :: Image t -> ImageInfo u -> IO u
getImageInfo (Image mem _ _) imgInfo = 
    -- type inference is made of win and awesome
    case imgInfo of
      ImageFormat      -> get
      ImageElementSize -> get
      ImageRowPitch    -> get
      ImageSlicePitch  -> get
      ImageWidth       -> get
      ImageDepth       -> get
      ImageHeight      -> get
    where get :: forall t. Storable t => IO t
          get = getStorable (clGetImageInfo mem (value imgInfo))

retainImage, releaseImage :: Image t -> IO ()
retainImage (Image img _ _) = handleCLError "retainImage" (clRetainMemObject img)
releaseImage (Image img _ _) = handleCLError "releaseImage" (clReleaseMemObject img)

-- TODO: Samplers

createProgramWithSource :: Context -> [String] -> IO Program
createProgramWithSource context sources = 
    allocaArray nSources $ \ sourceStrings ->
        do mapM_ (\(str, n) -> newCString str >>= pokeElemOff sourceStrings n) (zip sources [0..nSources])
           p <- handleCLCreationError "createProgramWithSource" $ clCreateProgramWithSource context (fromIntegral nSources) sourceStrings nullPtr
           mapM_ (\n -> peekElemOff sourceStrings n >>= free) [0..nSources]
           return p
    where nSources = length sources

-- TODO: createProgramWithBinary

retainProgram, releaseProgram :: Program -> IO ()
retainProgram = handleCLError "retainProgram" . clRetainProgram
releaseProgram = handleCLError "releaseProgram" . clReleaseProgram

buildProgram :: Program -> [Device] -> String -> IO ()
buildProgram prog devices options =
    withArray devices $ \ devArray ->
        withCString options $ \ optStr ->
            handleCLError "buildProgram" $ clBuildProgram prog (fromIntegral $ length devices) devArray optStr nullFunPtr nullPtr

-- Ignoring the callback parameters since we can take advantage of scoping and closures when
-- building the callback.
nonBlockingBuildProgram :: Program -> [Device] -> String -> IO () -> IO ()
nonBlockingBuildProgram prog devices options callback =
    withArray devices $ \ devArray -> 
    withCString options $ \ optStr ->
        do callback' <- makeBuildProgramCallback (const (const callback))
           handleCLError "nonBlockingBuildProgram" $ clBuildProgram prog (fromIntegral $ length devices) devArray optStr callback' nullPtr

unloadCompiler :: IO ()
unloadCompiler = clUnloadCompiler

getProgramInfo :: Program -> ProgramInfo t -> IO t
getProgramInfo program progInfo = 
    case progInfo of
      ProgramReferenceCount -> stoHelper
      ProgramContext        -> stoHelper
      ProgramNumDevices     -> stoHelper
      ProgramDevices        -> arrayHelper
      ProgramSource         -> stringHelper
      ProgramBinarySizes    -> arrayHelper
      ProgramBinaries       -> arrayHelper
    where getter          :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
          stoHelper       :: forall t. Storable t => IO t
          arrayHelper     :: forall t. Storable t => IO [t]

          getter       = clGetProgramInfo program (value progInfo)
          stoHelper    = getStorable getter
          stringHelper = getString getter
          arrayHelper  = getArray getter

getProgramBuildInfo :: Program -> Device -> ProgramBuildInfo t -> IO t
getProgramBuildInfo program device progBuildInfo = 
    case progBuildInfo of 
      ProgramBuildStatus  -> constEnumHelper
      ProgramBuildOptions -> stringHelper
      ProgramBuildLog     -> stringHelper
    where getter          :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
          constEnumHelper :: forall t u. (Enum t, Bounded t, Const t u, Eq u, Storable u, Show u) => IO t

          getter          = clGetProgramBuildInfo program device (value progBuildInfo)
          constEnumHelper = getConstEnum getter
          stringHelper    = getString getter

createKernel :: Program -> String -> IO Kernel
createKernel program name = 
    withCString name $ \ cName -> handleCLCreationError "createKernel" $ clCreateKernel program cName

createKernelsInProgram :: Program -> IO [Kernel]
createKernelsInProgram program = getCountedArray (clCreateKernelsInProgram program)

retainKernel, releaseKernel :: Kernel -> IO ()
retainKernel = handleCLError "retainKernel" . clRetainKernel
releaseKernel = handleCLError "releaseKernel" . clReleaseKernel

setKernelArg :: forall t. Storable t => Kernel -> Int -> t -> IO ()
setKernelArg kernel n val = 
    alloca $ \ valPtr -> 
        do poke valPtr val
           handleCLError "setKernelArg" $ clSetKernelArg kernel (fromIntegral n) (fromIntegral (sizeOf (undefined :: t))) valPtr

setKernelArgL :: forall t. Storable t => Kernel -> Int -> [t] -> IO ()
setKernelArgL kernel n vals = withArray vals $ handleCLError "setKernelArgL" . clSetKernelArg kernel (fromIntegral n) size
    where size = fromIntegral $ length vals * sizeOf (undefined :: t)

getKernelInfo :: Kernel -> KernelInfo t -> IO t
getKernelInfo kernel kInfo =
    case kInfo of
      KernelFunctionName -> stringHelper
      KernelNumArgs -> stoHelper
      KernelReferenceCount -> stoHelper
      KernelContext -> stoHelper
      KernelProgram -> stoHelper
    where getter          :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
          stoHelper       :: forall t. Storable t => IO t

          getter = clGetKernelInfo kernel (value kInfo)
          stringHelper = getString getter
          stoHelper = getStorable getter

getKernelWorkGroupInfo :: Kernel -> Device -> KernelWorkGroupInfo t -> IO t
getKernelWorkGroupInfo kernel device kwgInfo = 
    case kwgInfo of
      KernelWorkGroupSize                  -> stoHelper
      KernelCompileWorkGroupSize           -> arrayHelper
      KernelLocalMemSize                   -> stoHelper
      KernelPreferredWorkGroupSizeMultiple -> stoHelper
      KernelPrivateMemSize                 -> stoHelper
    where getter          :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
          stoHelper       :: forall t. Storable t => IO t
          arrayHelper     :: forall t. Storable t => IO [t]

          getter      = clGetKernelWorkGroupInfo kernel device (value kwgInfo)
          stoHelper   = getStorable getter
          arrayHelper = getArray getter

enqueueNDRangeKernel :: CommandQueue -> Kernel -> [Int] -> [Int] -> [Int] -> [Event] -> IO Event
enqueueNDRangeKernel queue k offset global local waitEvents =
    withArray' (map fromIntegral offset) $ \ offsetA ->
    withArray' (map fromIntegral global) $ \ globalA ->
    withArray' (map fromIntegral local) $ \ localA ->
        handleCLEventDriven "enqueueNDRangeKernel" (clEnqueueNDRangeKernel queue k (fromIntegral $ length global) offsetA globalA localA) waitEvents
    where withArray' [] c = c nullPtr
          withArray' xs c = withArray xs c

enqueueTask :: CommandQueue -> Kernel -> [Event] -> IO Event
enqueueTask queue k = handleCLEventDriven "enqueueTask" (clEnqueueTask queue k)

enqueueNativeKernel :: CommandQueue -> IO () -> [Event] -> IO Event
enqueueNativeKernel queue k waitEvents = 
    do k' <- makeNativeKernel k
       handleCLEventDriven "enqueueNativeKernel" (clEnqueueNativeKernel queue k' nullPtr 0 0 nullPtr nullPtr) waitEvents

--
-- Events
--

createUserEvent :: Context -> IO Event
createUserEvent = handleCLCreationError "createUserEvent" . clCreateUserEvent

setUserEventStatus :: Event -> EventStatus -> IO ()
setUserEventStatus event = handleCLError "setUserEventStatus" . clSetUserEventStatus event . value

waitForEvents :: [Event] -> IO ()
waitForEvents events = 
    withArray events $ \ evArray -> handleCLError "waitForEvents" $ clWaitForEvents (fromIntegral $ length events) evArray

getEventInfo :: Event -> EventInfo t -> IO t
getEventInfo event evInfo = 
    case evInfo of
      EventCommandQueue           -> stoHelper
      EventContext                -> stoHelper
      EventCommandType            -> constEnumHelper
      EventCommandExecutionStatus -> eventStatusFromCLInt `fmap` stoHelper 
      EventReferenceCount         -> stoHelper
    where getter          :: forall t. Storable t => SizeT -> Ptr t -> Ptr SizeT -> IO CLInt
          stoHelper       :: forall t. Storable t => IO t
          constEnumHelper :: forall t u. (Enum t, Bounded t, Const t u, Eq u, Storable u, Show u) => IO t

          getter = clGetEventInfo event (value evInfo)
          stoHelper = getStorable getter
          constEnumHelper = getConstEnum getter

eventCallback :: EventCallback a -> CLEventCallback a
eventCallback callback event clint = callback event (eventStatusFromCLInt clint)

setEventCallback :: Storable a => Event -> EventCallback a -> a -> IO ()
setEventCallback event callBack userData = 
    do dataPtr <- malloc 
       poke dataPtr userData
       callbackPtr <- makeEventCallback (eventCallback callBack)
       handleCLError "setEventCallback" $ clSetEventCallback event 0 callbackPtr dataPtr

setDatalessEventCallback :: Event -> EventCallback t -> IO ()
setDatalessEventCallback event callBack = 
    do callbackPtr <- makeEventCallback (eventCallback callBack)
       handleCLError "setDatalessEventCallback" $ clSetEventCallback event 0 callbackPtr nullPtr


retainEvent, releaseEvent :: Event -> IO ()
retainEvent = handleCLError "retainEvent" . clRetainEvent
releaseEvent = handleCLError "releaseEvent" . clReleaseEvent

enqueueMarker :: CommandQueue -> IO Event
enqueueMarker queue = 
    alloca $ \ evPtr -> 
        do handleCLError "enqueueMarker" (clEnqueueMarker queue evPtr)
           peek evPtr

enqueueBarrier :: CommandQueue -> IO ()
enqueueBarrier = handleCLError "enqueueBarrier" . clEnqueueBarrier

enqueueWaitForEvents :: CommandQueue -> [Event] -> IO ()
enqueueWaitForEvents queue events = 
    withArray events $ \ evArray -> handleCLError "enqueueWaitForEvents" $ clEnqueueWaitForEvents queue (fromIntegral $ length events) evArray

getEventProfilingInfo :: Event -> ProfilingInfo t -> IO t
getEventProfilingInfo event profInfo =
    case profInfo of
      ProfilingCommandQueued -> get
      ProfilingCommandSubmit -> get
      ProfilingCommandStart  -> get
      ProfilingCommandEnd    -> get
    where get :: forall t. Storable t => IO t
          get = getStorable (clGetEventProfilingInfo event (value profInfo))

flush, finish :: CommandQueue -> IO ()
flush = handleCLError "flush" . clFlush
finish = handleCLError "finish" . clFinish
