{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE DeriveDataTypeable, UndecidableInstances, GADTs #-}
module Language.OpenCL.Host.Constants where

import Data.Bits
import Foreign
import Foreign.C
import Unsafe.Coerce

import Data.Typeable
import Data.Data

unsupported = error "unsupported"

--
-- Support for the rest of the file
--

class Const t u | t -> u
    where value :: t -> u

data BitSet t 
    where BitSet :: (Const t u, Bits u, Integral u) => u -> BitSet t

instance (Const t u, Enum t, Bounded t, Show t) => Show (BitSet t)
    where show b = "bitSet " ++ show (allSetIn b) 

bitSet :: (Const t u, Bits u, Integral u) => [t] -> BitSet t
bitSet = foldr setBits (BitSet 0) 

setBits :: Const t u => t -> BitSet t -> BitSet t
setBits t (BitSet v) = BitSet (v .|. value t)

setIn :: Const t u => t -> BitSet t -> Bool
setIn t (BitSet v) = (v .&. value t) /= 0 

allSetIn :: (Const t u, Enum t, Bounded t) => BitSet t -> [t]
allSetIn bits = filter (`setIn` bits) (enumFromTo minBound maxBound)

valueFrom :: (Const t u, Integral u) => BitSet t -> u
valueFrom (BitSet v) = fromIntegral v -- seriously, GHC? Seriously?

--
-- Common CL types
--

type SizeT      = Word32
type CLInt      = Int32
type CLUInt     = Word32
type CLULong    = Word64
type CLBitField = CLULong

-- 
-- Platforms
--

data Platform_
type Platform = Ptr Platform_

type CLPlatformInfo = Word32

data PlatformInfo t
    where PlatformProfile    :: PlatformInfo String
          PlatformVersion    :: PlatformInfo String
          PlatformName       :: PlatformInfo String
          PlatformVendor     :: PlatformInfo String
          PlatformExtensions :: PlatformInfo String


instance Const (PlatformInfo t) CLPlatformInfo
    where value PlatformProfile    = 0x0900
          value PlatformVersion    = 0x0901
          value PlatformName       = 0x0902
          value PlatformVendor     = 0x0903
          value PlatformExtensions = 0x0904

-- split these into a separate file?  Also should probably include someone besides AMD...
amdVendorStrings = [ "Advanced Micro Devices, Inc.", "AuthenticAMD" ]

data DeviceType = DefaultDevice | CPU | GPU | APU | AllDevices 
                deriving (Bounded, Enum, Eq, Ord, Show, Typeable, Data)

instance Const DeviceType CLDeviceType
    where value DefaultDevice = 1 `shiftL` 0
          value CPU           = 1 `shiftL` 1
          value GPU           = 1 `shiftL` 2
          value APU           = 1 `shiftL` 3
          value AllDevices    = 0xFFFFFFFF

--
-- Devices
--

data Device_
type Device = Ptr Device_

type CLDeviceType = CLBitField

data CLBaseType = Char | Short | Int | Long | Float | Double | Half deriving (Bounded, Enum, Eq, Ord, Show)

data DeviceFPConfig = FPDenorm | FPInfNAN | FPRoundToNearest | FPRoundToZero | FPRoundToInf
    | FPFMA | FPSoftFloat deriving (Bounded, Enum, Eq, Ord, Show)

instance Const DeviceFPConfig CLBitField
    where value FPDenorm         = 1
          value FPInfNAN         = 1 `shiftL` 1
          value FPRoundToNearest = 1 `shiftL` 2
          value FPRoundToZero    = 1 `shiftL` 3
          value FPRoundToInf     = 1 `shiftL` 4
          value FPFMA            = 1 `shiftL` 5
          value FPSoftFloat      = 1 `shiftL` 6
  
data DeviceMemCacheType = NoCache | ReadOnlyCache | ReadWriteCache deriving (Bounded, Enum, Eq, Ord, Show)

instance Const DeviceMemCacheType CLUInt
    where value NoCache        = 0
          value ReadOnlyCache  = 1
          value ReadWriteCache = 2

data DeviceLocalMemType = Local | Global deriving (Bounded, Enum, Eq, Ord, Show)

instance Const DeviceLocalMemType CLUInt
    where value Local  = 1
          value Global = 2

data DeviceExecutionCapabilities = CLKernel | NativeKernel deriving (Bounded, Enum, Eq, Ord, Show)
  
instance Const DeviceExecutionCapabilities CLBitField
    where value CLKernel     = 1
          value NativeKernel = 1 `shiftL` 1

-- And here.. we.. go!
data DeviceInfo t
    where DeviceType                     :: DeviceInfo (BitSet DeviceType)
          DeviceVendorID                 :: DeviceInfo CLUInt
          DeviceMaxComputeUnits          :: DeviceInfo CLUInt
          DeviceMaxWorkItemDimensions    :: DeviceInfo CLUInt
          DeviceMaxWorkItemSizes         :: DeviceInfo [SizeT]
          DeviceMaxWorkGroupSize         :: DeviceInfo SizeT
          DevicePreferredVectorWidth     :: CLBaseType -> DeviceInfo CLUInt
          DeviceNativeVectorWidth        :: CLBaseType -> DeviceInfo CLUInt
          DeviceMaxClockFrequency        :: DeviceInfo CLUInt
          DeviceAddressBits              :: DeviceInfo CLUInt

          DeviceMaxMemAllocSize          :: DeviceInfo CLULong 

          DeviceImageSupport             :: DeviceInfo Bool
          DeviceMaxReadImageArgs         :: DeviceInfo CLUInt
          DeviceMaxWriteImageArgs        :: DeviceInfo CLUInt
          DeviceImage2DMaxWidth          :: DeviceInfo SizeT
          DeviceImage2DMaxHeight         :: DeviceInfo SizeT
          DeviceImage3DMaxWidth          :: DeviceInfo SizeT
          DeviceImage3DMaxHeight         :: DeviceInfo SizeT
          DeviceImage3DMaxDepth          :: DeviceInfo SizeT
          DeviceMaxSamplers              :: DeviceInfo CLUInt 

          DeviceMaxParameterSize         :: DeviceInfo SizeT 

          DeviceMemBaseAddrAlign         :: DeviceInfo CLUInt
          DeviceMinDataTypeAlignSize     :: DeviceInfo CLUInt 

          DeviceSingleFPConfig           :: DeviceInfo (BitSet DeviceFPConfig)

          DeviceGlobalMemCacheType       :: DeviceInfo DeviceMemCacheType
          DeviceGlobalMemCachelineSize   :: DeviceInfo CLUInt
          DeviceGlobalMemCacheSize       :: DeviceInfo CLULong
          DeviceGlobalMemSize            :: DeviceInfo CLULong 

          DeviceMaxConstantBufferSize    :: DeviceInfo CLULong
          DeviceMaxConstantArgs          :: DeviceInfo CLUInt 

          DeviceLocalMemType             :: DeviceInfo DeviceLocalMemType
          DeviceLocalMemSize             :: DeviceInfo CLULong
          DeviceErrorCorrectionSupport   :: DeviceInfo Bool 

          DeviceHostUnifiedMemory        :: DeviceInfo Bool 
                                    
          DeviceProfilingTimerResolution :: DeviceInfo SizeT 

          DeviceEndianLittle             :: DeviceInfo Bool 

          DeviceAvailable                :: DeviceInfo Bool
          DeviceCompilerAvailable        :: DeviceInfo Bool 

          DeviceExecutionCapabilities    :: DeviceInfo (BitSet DeviceExecutionCapabilities)
         
          DeviceQueueProperties          :: DeviceInfo (BitSet CommandQueueProperty)

          DevicePlatform                 :: DeviceInfo Platform 

          DeviceName                     :: DeviceInfo String
          DeviceVendor                   :: DeviceInfo String
          DriverVersion                  :: DeviceInfo String
          DeviceProfile                  :: DeviceInfo String
          DeviceVersion                  :: DeviceInfo String
          DeviceOpenCLCVersion           :: DeviceInfo String
          DeviceExtensions               :: DeviceInfo String

instance Const (DeviceInfo t) CLUInt
    where value DeviceType                        = 0x1000
          value DeviceVendorID                    = 0x1001
          value DeviceMaxComputeUnits             = 0x1002
          value DeviceMaxWorkItemDimensions       = 0x1003
          value DeviceMaxWorkGroupSize            = 0x1004
          value DeviceMaxWorkItemSizes            = 0x1005
          value (DevicePreferredVectorWidth Half) = 0x1034
          value (DevicePreferredVectorWidth t)    = fromIntegral (0x1006 + fromEnum t)
          value (DeviceNativeVectorWidth t)       = fromIntegral (0x1036 + fromEnum t)
          value DeviceMaxClockFrequency           = 0x100C
          value DeviceAddressBits                 = 0x100D
          value DeviceMaxReadImageArgs            = 0x100E
          value DeviceMaxWriteImageArgs           = 0x100F
          value DeviceMaxMemAllocSize             = 0x1010
          value DeviceImage2DMaxWidth             = 0x1011
          value DeviceImage2DMaxHeight            = 0x1012
          value DeviceImage3DMaxWidth             = 0x1013
          value DeviceImage3DMaxHeight            = 0x1014
          value DeviceImage3DMaxDepth             = 0x1015
          value DeviceImageSupport                = 0x1016
          value DeviceMaxParameterSize            = 0x1017
          value DeviceMaxSamplers                 = 0x1018
          value DeviceMemBaseAddrAlign            = 0x1019
          value DeviceMinDataTypeAlignSize        = 0x101A
          value DeviceSingleFPConfig              = 0x101B
          value DeviceGlobalMemCacheType          = 0x101C
          value DeviceGlobalMemCachelineSize      = 0x101D
          value DeviceGlobalMemCacheSize          = 0x101E
          value DeviceGlobalMemSize               = 0x101F
          value DeviceMaxConstantBufferSize       = 0x1020
          value DeviceMaxConstantArgs             = 0x1021
          value DeviceLocalMemType                = 0x1022
          value DeviceLocalMemSize                = 0x1023
          value DeviceErrorCorrectionSupport      = 0x1024
          value DeviceHostUnifiedMemory           = 0x1035
          value DeviceProfilingTimerResolution    = 0x1025
          value DeviceEndianLittle                = 0x1026
          value DeviceAvailable                   = 0x1027
          value DeviceCompilerAvailable           = 0x1028
          value DeviceExecutionCapabilities       = 0x1029
          value DeviceQueueProperties             = 0x102A
          value DeviceName                        = 0x102B
          value DeviceVendor                      = 0x102C
          value DriverVersion                     = 0x102D
          value DeviceProfile                     = 0x102E
          value DeviceVersion                     = 0x102F
          value DeviceOpenCLCVersion              = 0x103D
          value DeviceExtensions                  = 0x1030
          value DevicePlatform                    = 0x1031

-- 
-- Contexts
--

-- At this point, I'm treating context property lists as being basically write-only.  I'd like
-- things to be (mostly) strongly typed on the way in---that is, you can't specify arbitrary values
-- as context properties or specify properties and values that don't match---but I'm not making any
-- attempt to make it possible to read back out of a list of context properties once it's created.
-- It might actually be possible assuming you knew what properties to look for and were willing to
-- do a fair amount of 'unsafeCoerce'ing, but I'm not doing that now.

-- The set of context properties seems to be a fairly extensible thing---for instance, the CL/GL
-- interop stuff adds to the list of context properties.  So, rather than hard-code the set of
-- possible properties, I'm using a class to identify those types that could be context properties.
-- The important thing about this class is the Const precondition; otherwise, it just serves as a
-- type-level tag.  Additionally, this will only work for types 'u' that can be 'unsafeCoerce'd into
-- 'CInt's, but I'm leaving that up to the types in 'ContextProperty' to ensure.

class Const (t u) CLContextProperty => ContextProperty t u

-- Rather than treat a list of context properties using some kind of existential pair 'exists t
-- u. ContextProperty t u => (t u, t)', I'm using the underlying representation of a list of
-- 'CInt's.  This makes it fantastically easy to pass a list of context properties to the runtime,
-- or get one back from the runtime---I just have to do the normal array marshalling stuff.  It
-- makes it less possible to actually read property-value pairs back out of a list of context
-- properties, but I don't see either a really good way to do that, or a really good reason.

newtype ContextProperties = CPs [CInt]

noProperties = CPs []

pushContextProperty :: ContextProperty t u => t u -> u -> ContextProperties -> ContextProperties
pushContextProperty prop val (CPs cps) = CPs (value prop : unsafeCoerce val : cps)

withContextProperties :: ContextProperties -> (Ptr CInt -> IO a) -> IO a
withContextProperties (CPs cps) comp = 
    allocaArray (length cps + 1) $ \ ptr ->
        do pokeArray ptr cps
           pokeElemOff ptr (length cps) (0 :: CInt)
           comp ptr

-- Now, I define the core properties required by the spec, of which there is one:

data CoreContextProperty t 
    where ContextPlatform :: CoreContextProperty Platform

instance Const (CoreContextProperty t) CLContextProperty 
    where value ContextPlatform = 0x1084

instance ContextProperty CoreContextProperty t

-- And that's all for context properties for now.  This interface works, but it has limitations.

data Context_
type Context = Ptr Context_

type CLContextProperty = CInt
type CLContextInfo = CLUInt

data ContextInfo t
    where ContextReferenceCount :: ContextInfo CLUInt
          ContextNumDevices     :: ContextInfo CLUInt
          ContextDevices        :: ContextInfo [Device]
          ContextProperties     :: ContextInfo ContextProperties

instance Const (ContextInfo t) CLUInt
    where value ContextReferenceCount = 0x1080
          value ContextDevices        = 0x1081
          value ContextProperties     = 0x1082
          value ContextNumDevices     = 0x1083

--
-- Command Queues
--

data CommandQueue_
type CommandQueue = Ptr CommandQueue_

type CLCommandQueueProperties = CLBitField
type CLCommandQueueInfo = CLUInt

data CommandQueueProperty = OutOfOrderExecution | Profiling deriving (Bounded, Enum, Eq, Ord, Show)

instance Const CommandQueueProperty CLCommandQueueProperties
    where value OutOfOrderExecution = 1
          value Profiling           = 1 `shiftL` 1

data CommandQueueInfo t
    where QueueContext        :: CommandQueueInfo Context
          QueueDevice         :: CommandQueueInfo Device
          QueueReferenceCount :: CommandQueueInfo CLUInt
          QueueProperties     :: CommandQueueInfo (BitSet CommandQueueProperty)

instance Const (CommandQueueInfo t) CLCommandQueueInfo
    where value QueueContext        = 0x1090
          value QueueDevice         = 0x1091
          value QueueReferenceCount = 0x1092
          value QueueProperties     = 0x1093

--
-- Buffers
--

data CLMem_
type CLMem = Ptr CLMem_

type CLMemFlags = CLBitField
type CLBufferCreateType = CLUInt

data CLBufferRegion = CLBufferRegion { origin, size :: SizeT } deriving (Eq, Show)

data MemFlag = ReadWrite | ReadOnly | WriteOnly | UseHostPtr | AllocHostPtr 
             | CopyHostPtr deriving (Bounded, Enum, Eq, Ord, Show)

instance Const MemFlag CLMemFlags
    where value ReadWrite    = 1 `shiftL` 0
          value WriteOnly    = 1 `shiftL` 1
          value ReadOnly     = 1 `shiftL` 2
          value UseHostPtr   = 1 `shiftL` 3
          value AllocHostPtr = 1 `shiftL` 4
          value CopyHostPtr  = 1 `shiftL` 5

data BufferCreateType = CreateTypeRegion deriving (Bounded, Enum, Eq, Ord, Show)

instance Const BufferCreateType CLBufferCreateType
    where value CreateTypeRegion = 0x1220

--
-- Images
--

type CLChannelOrder = CLUInt
type CLChannelType = CLUInt
type CLMemObjectType = CLUInt
type CLImageInfo = CLUInt
type CLMemInfo = CLUInt


data CLImageFormat = CLImageFormat { imageChannelOrder :: CLChannelOrder
                                   , imageChannelDataType :: CLChannelType } deriving (Eq, Show)

data ChannelOrder = R | A | RG | RA | RGB | RGBA | BGRA | ARGB | Intensity 
                  | Luminance | Rx | RGx | RGBx deriving (Bounded, Enum, Eq, Ord, Show)

instance Const ChannelOrder CLChannelOrder
    where value R         = 0x10B0
          value A         = 0x10B1
          value RG        = 0x10B2
          value RA        = 0x10B3
          value RGB       = 0x10B4
          value RGBA      = 0x10B5
          value BGRA      = 0x10B6
          value ARGB      = 0x10b7
          value Intensity = 0x10b8
          value Luminance = 0x10B9
          value Rx        = 0x10BA
          value RGx       = 0x10BB
          value RGBx      = 0x10BC

data ChannelType = ChanTypeSNormInt8      
                 | ChanTypeSNormInt16     
                 | ChanTypeUNormInt8      
                 | ChanTypeUNormInt16     
                 | ChanTypeUNormShort565  
                 | ChanTypeUNormShort555  
                 | ChanTypeUNormInt101010 
                 | ChanTypeSignedInt8     
                 | ChanTypeSignedInt16    
                 | ChanTypeSignedInt32    
                 | ChanTypeUnsignedInt8   
                 | ChanTypeUnsignedInt16  
                 | ChanTypeUnsignedInt32  
                 | ChanTypeHalfFloat      
                 | ChanTypeFloat          
                   deriving (Bounded, Enum, Eq, Ord, Show)

instance Const ChannelType CLChannelType
    where value ChanTypeSNormInt8      = 0x10D0
          value ChanTypeSNormInt16     = 0x10D1
          value ChanTypeUNormInt8      = 0x10D2
          value ChanTypeUNormInt16     = 0x10D3
          value ChanTypeUNormShort565  = 0x10D4
          value ChanTypeUNormShort555  = 0x10D5
          value ChanTypeUNormInt101010 = 0x10D6
          value ChanTypeSignedInt8     = 0x10D7
          value ChanTypeSignedInt16    = 0x10D8
          value ChanTypeSignedInt32    = 0x10D9
          value ChanTypeUnsignedInt8   = 0x10DA
          value ChanTypeUnsignedInt16  = 0x10DB
          value ChanTypeUnsignedInt32  = 0x10DC
          value ChanTypeHalfFloat      = 0x10DD
          value ChanTypeFloat          = 0x10DE



data MemObjectType = MemObjectBuffer | MemObjectImage2D | MemObjectImage3D deriving (Bounded, Enum, Eq, Ord, Show)

instance Const MemObjectType CLMemObjectType
    where value MemObjectBuffer  = 0x10F0
          value MemObjectImage2D = 0x10F1
          value MemObjectImage3D = 0x10F2

data ImageInfo t
    where ImageFormat      :: ImageInfo CLImageFormat
          ImageElementSize :: ImageInfo SizeT
          ImageRowPitch    :: ImageInfo SizeT
          ImageSlicePitch  :: ImageInfo SizeT
          ImageWidth       :: ImageInfo SizeT
          ImageDepth       :: ImageInfo SizeT
          ImageHeight      :: ImageInfo SizeT

instance Const (ImageInfo t) CLImageInfo
    where value ImageFormat      = 0x1110
          value ImageElementSize = 0x1111
          value ImageRowPitch    = 0x1112
          value ImageSlicePitch  = 0x1113
          value ImageWidth       = 0x1114
          value ImageHeight      = 0x1115
          value ImageDepth       = 0x1116

data MemInfo t
    where MemType                :: MemInfo MemObjectType
          MemFlags               :: MemInfo (BitSet MemFlag)
          MemSize                :: MemInfo SizeT
          MemHostPtr             :: MemInfo (Ptr a)
          MemMapCount            :: MemInfo CLUInt
          MemReferenceCount      :: MemInfo CLUInt
          MemContext             :: MemInfo Context
          MemAssociatedMemObject :: MemInfo CLMem
          MemOffset              :: MemInfo SizeT

instance Const (MemInfo t) CLMemInfo
    where value MemType                = 0x1100
          value MemFlags               = 0x1101
          value MemSize                = 0x1102
          value MemHostPtr             = 0x1103
          value MemMapCount            = 0x1104
          value MemReferenceCount      = 0x1105
          value MemContext             = 0x1106
          value MemAssociatedMemObject = 0x1107
          value MemOffset              = 0x1108

data AddressingMode = AddressNone | AddressClampToEdge | AddressClamp | AddressRepeat | AddressMirroredRepeat
                    deriving (Bounded, Enum, Eq, Ord, Show)

instance Const (AddressingMode) CLAddressingMode
    where value AddressNone           = 0x1130
          value AddressClampToEdge    = 0x1131
          value AddressClamp          = 0x1132
          value AddressRepeat         = 0x1133
          value AddressMirroredRepeat = 0x1134

data FilterMode = FilterNearest | FilterLinear deriving (Bounded, Enum, Eq, Ord, Show)

instance Const FilterMode CLFilterMode
    where value FilterNearest = 0x1140
          value FilterLinear  = 0x1141

--
-- Samplers
--

data Sampler_
type Sampler = Ptr Sampler_
type CLAddressingMode = CLUInt
type CLFilterMode = CLUInt
type CLSamplerInfo = CLUInt

data SamplerInfo t
    where SamplerReferenceCount   :: SamplerInfo CLUInt
          SamplerContext          :: SamplerInfo Context
          SamplerNormalizedCoords :: SamplerInfo Bool
          SamplerAddressingMode   :: SamplerInfo AddressingMode
          SamplerFilterMode       :: SamplerInfo FilterMode

instance Const (SamplerInfo t) CLSamplerInfo
    where value SamplerReferenceCount   = 0x1150
          value SamplerContext          = 0x1151
          value SamplerNormalizedCoords = 0x1152
          value SamplerAddressingMode   = 0x1153
          value SamplerFilterMode       = 0x1154

--
-- Programs
--


data Program_
type Program = Ptr Program_
type CLProgramInfo = CLUInt
type CLProgramBuildInfo = CLUInt

data ProgramInfo t
    where ProgramReferenceCount :: ProgramInfo CLUInt
          ProgramContext        :: ProgramInfo Context
          ProgramNumDevices     :: ProgramInfo CLUInt
          ProgramDevices        :: ProgramInfo [Device]
          ProgramSource         :: ProgramInfo String
          ProgramBinarySizes    :: ProgramInfo [SizeT]
          ProgramBinaries       :: ProgramInfo [Ptr CUChar]

instance Const (ProgramInfo t) CLProgramInfo
    where value ProgramReferenceCount = 0x1160
          value ProgramContext        = 0x1161
          value ProgramNumDevices     = 0x1162
          value ProgramDevices        = 0x1163
          value ProgramSource         = 0x1164
          value ProgramBinarySizes    = 0x1165
          value ProgramBinaries       = 0x1166

data BuildStatus = BuildNone | BuildError | BuildSuccess | BuildInProgress
                 deriving (Bounded, Enum, Eq, Ord, Show)
type CLBuildStatus = CLInt

instance Const BuildStatus CLBuildStatus
    where value BuildSuccess    = 0
          value BuildNone       = -1
          value BuildError      = -2
          value BuildInProgress = -3

data ProgramBuildInfo t
    where ProgramBuildStatus  :: ProgramBuildInfo BuildStatus
          ProgramBuildOptions :: ProgramBuildInfo String
          ProgramBuildLog     :: ProgramBuildInfo String

instance Const (ProgramBuildInfo t) CLProgramBuildInfo
    where value ProgramBuildStatus  = 0x1181
          value ProgramBuildOptions = 0x1182
          value ProgramBuildLog     = 0x1183

--
-- Kernels
--

data Kernel_
type Kernel = Ptr Kernel_
type CLKernelInfo = CLUInt
type CLKernelWorkGroupInfo = CLUInt

data KernelInfo t
    where KernelFunctionName   :: KernelInfo String
          KernelNumArgs        :: KernelInfo CLUInt
          KernelReferenceCount :: KernelInfo CLUInt
          KernelContext        :: KernelInfo Context
          KernelProgram        :: KernelInfo Program

instance Const (KernelInfo t) CLKernelInfo
    where value KernelFunctionName   = 0x1190
          value KernelNumArgs        = 0x1191
          value KernelReferenceCount = 0x1192
          value KernelContext        = 0x1193
          value KernelProgram        = 0x1194

data KernelWorkGroupInfo t
    where KernelWorkGroupSize                  :: KernelWorkGroupInfo SizeT
          KernelCompileWorkGroupSize           :: KernelWorkGroupInfo [SizeT]
          KernelLocalMemSize                   :: KernelWorkGroupInfo CLULong
          KernelPreferredWorkGroupSizeMultiple :: KernelWorkGroupInfo SizeT
          KernelPrivateMemSize                 :: KernelWorkGroupInfo CLULong

instance Const (KernelWorkGroupInfo t) CLKernelWorkGroupInfo
    where value KernelWorkGroupSize                  = 0x11B0
          value KernelCompileWorkGroupSize           = 0x11B1
          value KernelLocalMemSize                   = 0x11B2
          value KernelPreferredWorkGroupSizeMultiple = 0x11B3
          value KernelPrivateMemSize                 = 0x11B4

data CommandType = CommandNDRangeKernel    
                 | CommandTask             
                 | CommandNativeKernel     
                 | CommandReadBuffer       
                 | CommandWriteBuffer      
                 | CommandCopyBuffer       
                 | CommandReadImage        
                 | CommandWriteImage       
                 | CommandCopyImage        
                 | CommandCopyImageToBuffer
                 | CommandCopyBufferToImage
                 | CommandMapBuffer        
                 | CommandMapImage         
                 | CommandUnmapMemObject   
                 | CommandMarker           
                 | CommandAcquireGlObjects 
                 | CommandReleaseGlObjects 
                 | CommandReadBufferRect   
                 | CommandWriteBufferRect  
                 | CommandCopyBufferRect   
                 | CommandUser             
                   deriving (Bounded, Enum, Eq, Ord, Show)

type CLCommandType = CLUInt

instance Const CommandType CLCommandType
    where value CommandNDRangeKernel     = 0x11F0
          value CommandTask              = 0x11F1
          value CommandNativeKernel      = 0x11F2
          value CommandReadBuffer        = 0x11F3
          value CommandWriteBuffer       = 0x11F4
          value CommandCopyBuffer        = 0x11F5
          value CommandReadImage         = 0x11F6
          value CommandWriteImage        = 0x11F7
          value CommandCopyImage         = 0x11F8
          value CommandCopyImageToBuffer = 0x11F9
          value CommandCopyBufferToImage = 0x11FA
          value CommandMapBuffer         = 0x11FB
          value CommandMapImage          = 0x11FC
          value CommandUnmapMemObject    = 0x11FD
          value CommandMarker            = 0x11FE
          value CommandAcquireGlObjects  = 0x11FF
          value CommandReleaseGlObjects  = 0x1200
          value CommandReadBufferRect    = 0x1201
          value CommandWriteBufferRect   = 0x1202
          value CommandCopyBufferRect    = 0x1203
          value CommandUser              = 0x1204

--
-- Events
--

data Event_
type Event = Ptr Event_
type CLEventInfo = CLUInt
type EventCallback a = Event -> EventStatus -> Ptr a -> IO ()
type CLEventCallback a = Event -> CLInt -> Ptr a -> IO ()
type CLProfilingInfo = CLUInt

data EventStatus = Complete | Running | Submitted | Queued | Errored Int deriving (Eq, Show)

instance Const EventStatus CLInt
    where value Complete    = 0
          value Running     = 1
          value Submitted   = 2
          value Queued      = 3
          value (Errored n) = fromIntegral n

eventStatusFromCLInt :: CLInt -> EventStatus
eventStatusFromCLInt 0             = Complete
eventStatusFromCLInt 1             = Running
eventStatusFromCLInt 2             = Submitted
eventStatusFromCLInt 3             = Queued
eventStatusFromCLInt n | n < 0     = Errored (fromIntegral n)
                       | otherwise = error $ "Uninterpreted event status in eventStatusFromCLInt: " ++ show n
                         
data EventInfo t
    where EventCommandQueue           :: EventInfo CommandQueue
          EventContext                :: EventInfo Context
          EventCommandType            :: EventInfo CommandType
          EventCommandExecutionStatus :: EventInfo EventStatus
          EventReferenceCount         :: EventInfo CLUInt

instance Const (EventInfo t) CLEventInfo
    where value EventCommandQueue           = 0x11D0
          value EventCommandType            = 0x11D1
          value EventReferenceCount         = 0x11D2
          value EventCommandExecutionStatus = 0x11D3
          value EventContext                = 0x11D4

data ProfilingInfo t
    where ProfilingCommandQueued :: ProfilingInfo CLULong
          ProfilingCommandSubmit :: ProfilingInfo CLULong
          ProfilingCommandStart  :: ProfilingInfo CLULong
          ProfilingCommandEnd    :: ProfilingInfo CLULong

instance Const (ProfilingInfo t) CLProfilingInfo
    where value ProfilingCommandQueued = 0x1280
          value ProfilingCommandSubmit = 0x1281
          value ProfilingCommandStart  = 0x1282
          value ProfilingCommandEnd    = 0x1283