{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, OverlappingInstances, MultiParamTypeClasses, GADTs, ParallelListComp, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module Language.OpenCL.Host (
    module Language.OpenCL.Host
  , module Language.OpenCL.Host.Constants
  , Buffer
  , C.Error(..)
  , C.CLError(..)
  , liftIO, MonadIO)  where

import Language.OpenCL.Host.Constants hiding (Kernel, Program, size)
import qualified Language.OpenCL.Host.Constants as Co
import Language.OpenCL.Host.FFI as F
import Language.OpenCL.Host.Core (Buffer)
import qualified Language.OpenCL.Host.Core as C

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception as Except
import Control.Exception (Exception)
import Control.Monad.Reader
import Data.Array.Storable
import Data.IORef
import Foreign.Storable
import Prelude hiding (catch)
import Unsafe.Coerce
import System.IO.Unsafe

----------------------------------------------------------------------------------------------------
-- Lift IO exception into CL monads
--
-- Inspired by a conversation on the Haskell mailing list [1].  There's a more heavyweight
-- implementation of this idea in Hackage [2], but since I only need it for a few operations and for
-- one transformer class, I chose to reimplement the relevant parts.
--
-- [1] http://www.haskell.org/pipermail/libraries/2008-February/009171.html
-- [2] http://hackage.haskell.org/package/MonadCatchIO-mtl
----------------------------------------------------------------------------------------------------

puts :: MonadIO m => String -> m ()
puts = liftIO . putStrLn

class MonadIO m => CatchIO m 
    where catch :: Exception e => m t -> (e -> m t) -> m t
          -- Not sure how much we need the next two, honestly
          block :: m t -> m t
          unblock :: m t -> m t

appendingLocation :: CatchIO m => String -> m t -> m t
appendingLocation location comp = comp `catch` appender
    where appender err = liftIO $ throw (C.appendLocation location err)

instance CatchIO IO
    where catch = Except.catch
          block = Except.block
          unblock = Except.unblock

instance CatchIO m => CatchIO (ReaderT r m)
    where catch m catcher = ReaderT $ \ val ->
                            runReaderT m val `catch` \ exception -> 
                                runReaderT (catcher exception) val
          block m = ReaderT $ \ val -> block (runReaderT m val)
          unblock m = ReaderT $ \ val -> unblock (runReaderT m val)

throw :: (Exception e, MonadIO m) => e -> m t
throw = liftIO . Except.throw

finally :: CatchIO m => m t -> m u -> m t
finally c c' = 
    do v <- c `catch` (\ex -> do c'; throw (ex :: Except.SomeException)) -- if only scoped type variables worked here.. but the type of ex is not rigid.
       c'
       return v

----------------------------------------------------------------------------------------------------
-- Polite operations
--
-- This name is fairly bad, but I haven't thought of a better one yet.  It wraps the notion of
-- operations that can potentially wait for events before running.  "polite" operations already have
-- a notion of waiting for events, so we don't need to add on.  "impolite" operations do not, so we
-- insert calls to waitForEvents if necessary.
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
-- PoliteT transformer and instances

data PoliteT m t = Polite ([Event] -> m t)

polite :: ([Event] -> m t) -> PoliteT m t
polite = Polite

impolite :: MonadIO m => (m t) -> PoliteT m t
impolite c = Polite (\events -> if null events
                                then c
                                else liftIO (C.waitForEvents events) >> c)

runPolite :: MonadIO m => PoliteT m t -> m t
runPolite (Polite c) = c []

preservingPoliteness :: (m t -> n t) -> PoliteT m t -> PoliteT n t
preservingPoliteness transformer (Polite f) = Polite (\events -> transformer (f events))

instance Functor m => Functor (PoliteT m)
    where fmap f (Polite c) = Polite (\events -> fmap f (c events))

instance MonadIO m => Monad (PoliteT m)
    where return x                = impolite (return x)
          Polite c >>= f   = Polite (\events -> c events >>= runPolite . f) 

instance MonadIO m => MonadIO (PoliteT m)
    where liftIO = impolite . liftIO

instance CatchIO m => CatchIO (PoliteT m)
    where catch (Polite c) catcher   = Polite (\events -> c events `catch` (runPolite . catcher))
          block (Polite c)           = Polite (\events -> block (c events))
          unblock (Polite c)         = Polite (\events -> unblock (c events))

----------------------------------------------------------------------------------------------------
-- Class of polite monads

class MonadPolite m
    where after :: m t -> [Event] -> m t

instance MonadPolite (PoliteT m)
    where Polite c `after` events = Polite (\events' -> c (events ++ events'))

instance (MonadIO m, MonadReader t m) => MonadReader t (PoliteT m)
    where ask                = impolite ask
          local f (Polite c) = polite (\events -> local f (c events))

----------------------------------------------------------------------------------------------------
-- Monad stack
----------------------------------------------------------------------------------------------------

newtype ContextM t = ContextM { runContextM :: PoliteT (ReaderT Context IO) t }
    deriving (Functor, Monad, MonadIO, CatchIO, MonadReader Context, MonadPolite)

class (Functor m, MonadIO m) => Contextual m 
    where contextual :: ContextM t -> m t

instance Contextual ContextM
    where contextual = id

theContext :: Contextual m => m Context
theContext = contextual ask

queryContext :: (Queryable Context q, Contextual m) => q a -> m a
queryContext q = theContext >>= (? q)

newtype QueueM t = QueueM { runQueueM :: PoliteT (ReaderT CommandQueue (ReaderT Context IO)) t }
    deriving (Functor, Monad, MonadIO, CatchIO, MonadReader CommandQueue, MonadPolite)

class (Contextual m, MonadIO m) => Queued m 
    where queued :: QueueM t -> m t

instance Contextual QueueM
    where contextual (ContextM c) = QueueM (preservingPoliteness lift c)

instance Queued QueueM
    where queued = id

theQueue :: Queued m => m CommandQueue
theQueue = queued ask

queryQueue :: (Queryable CommandQueue q, Queued m) => q a -> m a
queryQueue q = theQueue >>= (? q)

queuedPolite :: Queued m => ([Event] -> ReaderT CommandQueue (ReaderT Context IO) t) -> m t
queuedPolite = queued . QueueM . polite

----------------------------------------------------------------------------------------------------
-- Monad wrapping
----------------------------------------------------------------------------------------------------

class Wraps t m n | t -> m
    where with :: t -> m u -> n u

----------------------------------------------------------------------------------------------------
-- Object lifespan management, not at all inspired by C#
----------------------------------------------------------------------------------------------------

class Lifespan t
    where retain :: MonadIO m => t -> m ()
          release :: MonadIO m => t -> m ()

instance Lifespan t => Lifespan [t]
    where retain = mapM_ retain
          release = mapM_ release

instance Lifespan t => Lifespan (Maybe t)
    where retain = maybe (return ()) retain
          release = maybe (return ()) release

-- 'using' calls 'release' on the object generated by 'ctor' when 'comp' finishes; this assumes that
-- 'ctor' has implicitly retained its result.
using :: (Lifespan t, CatchIO m) => m t -> (t -> m u) -> m u
using ctor comp = do val <- ctor
                     comp val `finally` release val

-- 'retaining' begins by calling 'retain' on its first argument; it calls 'release' when 'comp'
-- finishes.
retaining :: (Lifespan t, CatchIO m) => t -> m u -> m u
retaining obj comp = do retain obj
                        comp `finally` release obj

-- 'withNew' combines a 'with' and 'using'; it assumes that 'ctor' will perform a 'retain' on its
-- result.
withNew :: (Wraps t m n, Lifespan t, CatchIO n) => n t -> m u -> n u
withNew ctor block = using ctor (flip with block)

----------------------------------------------------------------------------------------------------
-- Queryable properties
----------------------------------------------------------------------------------------------------

class Queryable t qt
    where (?) :: MonadIO m => t -> qt u -> m u

----------------------------------------------------------------------------------------------------
-- Platforms
----------------------------------------------------------------------------------------------------

platforms :: MonadIO m => m [Platform]
platforms = liftIO C.getPlatforms

instance Queryable Platform PlatformInfo
    where platform ? info = liftIO $ C.getPlatformInfo platform info

----------------------------------------------------------------------------------------------------
-- Devices
----------------------------------------------------------------------------------------------------

devices :: MonadIO m => Platform -> m [Device]
devices p = liftIO $ C.getDevices p (bitSet [AllDevices])

devicesOfType :: MonadIO m => Platform -> [DeviceType] -> m [Device]
devicesOfType p types = liftIO $ C.getDevices p (bitSet types)

instance Queryable Device DeviceInfo
    where device ? info = liftIO $ C.getDeviceInfo device info

----------------------------------------------------------------------------------------------------
-- Contexts
----------------------------------------------------------------------------------------------------

context :: MonadIO m => Platform -> [Device] -> m Context
context p = contextFromProperties (pushContextProperty ContextPlatform p noProperties)

contextFromType :: MonadIO m => Platform -> [DeviceType] -> m Context
contextFromType p = contextFromPropertiesAndType (pushContextProperty ContextPlatform p noProperties)


contextFromProperties :: MonadIO m => ContextProperties -> [Device] -> m Context
contextFromProperties cprops ds = liftIO $ C.createContext cprops ds

contextFromPropertiesAndType :: MonadIO m => ContextProperties -> [DeviceType] -> m Context
contextFromPropertiesAndType cprops dtypes = liftIO $ C.createContextFromType cprops (bitSet dtypes)

instance Queryable Context ContextInfo
    where context ? info = liftIO $ C.getContextInfo context info

instance Queryable Context CoreContextProperty
    where context ? ContextPlatform = 
              do CPs props <- context ? ContextProperties
                 return (findPlatform props)
              where findPlatform []                      = error "Unable to find platform in context properties.. wtf?"
                    findPlatform (pname : pval : rest) 
                        | pname == value ContextPlatform = unsafeCoerce pval
                        | otherwise                      = findPlatform rest

instance MonadIO m => Wraps Context ContextM m 
    where with context comp = liftIO $ runReaderT (runPolite (runContextM comp)) context

instance Lifespan Context
    where retain = liftIO . C.retainContext
          release = liftIO . C.releaseContext

----------------------------------------------------------------------------------------------------
-- Command Queues
----------------------------------------------------------------------------------------------------

queue :: Contextual m => Device -> m CommandQueue
queue d = contextual $ do c <- theContext
                          liftIO $ C.createCommandQueue c d (bitSet [])

queueWithProperties :: Contextual m => Device -> [CommandQueueProperty] -> m CommandQueue
queueWithProperties d props = contextual $ do c <- theContext
                                              liftIO $ C.createCommandQueue c d (bitSet props)

instance Queryable CommandQueue CommandQueueInfo
    where q ? info = liftIO $ C.getCommandQueueInfo q info

instance Contextual m => Wraps CommandQueue QueueM m
    where with queue = contextual . ContextM . preservingPoliteness (flip runReaderT queue) . runQueueM

instance Lifespan CommandQueue
    where retain = liftIO . C.retainCommandQueue
          release = liftIO . C.releaseCommandQueue

----------------------------------------------------------------------------------------------------
-- Read/Write

class Readable cl hs 
    where readFrom :: (Storable t, Queued m) => cl t -> Int -> Int -> m (hs t)

class Writable cl hs
    where writeTo :: (Storable t, Queued m) => cl t -> Int -> hs t -> m Event

----------------------------------------------------------------------------------------------------
-- Buffers
--
-- As with the version in L.O.H.Core, this API is a work in progress.  In particular, I'm interested
-- in MemFlags... it seems like there are probably some higher level abstractions that would
-- motivate the use of particular combinations of flags, and I'd like to look at expressing those
-- rather than exposing lists of flags.
----------------------------------------------------------------------------------------------------

buffer :: (Storable t, Contextual m) => Int -> m (Buffer t)
buffer size = contextual $ do c <- theContext; liftIO $ C.createBuffer c (bitSet []) size
                              
bufferWithFlags :: (Storable t, Contextual m) => Int -> [MemFlag] -> m (Buffer t)
bufferWithFlags size flags = contextual $ do c <- theContext; liftIO $ C.createBuffer c (bitSet flags) size

-- TODO: I don't really know the uses for createSubBuffer yet, so I'm just lifting the Core version
-- through Contextual
subBuffer :: (Storable t, Contextual m) => Buffer t -> Int -> Int -> [MemFlag] -> m (Buffer t)
subBuffer buffer start length flags = contextual $ liftIO $ C.createSubBuffer buffer (bitSet flags) start length

instance Queryable (Buffer t) MemInfo
    where buffer ? info = liftIO $ C.getBufferInfo buffer info

instance Lifespan (Buffer t)
    where retain = liftIO . C.retainBuffer
          release = liftIO . C.releaseBuffer

-- TODO: the following doesn't work.

-- Here's where I really start going out on a limb.  Rather than exposing the blocking and
-- non-blocking read and write calls from the C API (and the Core module), I'm going to expose
-- non-blocking, event-returning reads and writes.  If the user wants to wait for the memory
-- operation to finish, they can either wait for the event or attach a continuation to it (more on
-- that when I get to events).  For writes, this seems like it makes a lot of sense.  The allocation
-- isn't actually under the user's control anyway---I'm doing the allocation automatically in the
-- Core functions, so I can't see the huge benefit in a blocking write.  On the other hand,
-- non-blocking read is a bit sketchier.  After all, if someone actually WANTS that data, they'll
-- presumably want to wait until it's there.  I'm hoping to handle that part with lazy-IO.

-- There are a few remaining conceptual TODOs here.  First, the whole thing about specifying starts
-- and ranges on each read and write seems bogus to me.  I wonder whether there's a way to decompose
-- that into separate operations to restrict buffers and to read/write to them.  This would seem to
-- tie together with the parts I don't understand about subbuffers above.  The [Event] -> IO Event
-- stuff is also begging to be put into some kind of monad, but I'm not quite sure how to do
-- that---trying to expose a task-parallel monad that's distinct from the existing monad stack seems
-- to make the most sense, but I'm not sure how to integrate that with the other monads.

instance Readable Buffer []
    where readFrom buffer start range = 
              queuedPolite $ \events ->
                  do q <- ask
                     var <- liftIO $ newEmptyMVar
                     ev <- liftIO $ C.enqueueNonBlockingReadBuffer q buffer start range (callback var) events
                     liftIO $ C.waitForEvents [ev]
                     vals <- liftIO . unsafeInterleaveIO $ takeMVar var
                     return vals
                  where callback var vals = putMVar var vals

instance Readable Buffer (StorableArray Int)
    where readFrom buffer start range = 
              queuedPolite $ \events -> do q <- ask; liftIO $ C.enqueueBlockingReadBufferSA q buffer start range events

instance Writable Buffer []
    where writeTo buffer start values = 
              queuedPolite $ \events -> do q <- ask; liftIO $ C.enqueueNonBlockingWriteBuffer q buffer start values events

instance Writable Buffer (StorableArray Int)
    where writeTo buffer start values = 
              queuedPolite $ \events -> do q <- ask; liftIO $ C.enqueueBlockingWriteBufferSA q buffer start values events                

copyBuffer :: (Storable t, Queued m) => Buffer t -> Buffer t -> Int -> Int -> Int -> m Event
copyBuffer from to fromOrigin toOrigin length =
    queuedPolite $ \events -> do q <- ask; liftIO $ C.enqueueCopyBuffer q from to fromOrigin toOrigin length events

----------------------------------------------------------------------------------------------------
-- Images: waiting for now...
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
-- Programs
----------------------------------------------------------------------------------------------------

data Program = Program { program :: Co.Program
                       , built :: MVar Bool }

programFromSource :: Contextual m => String -> m Program
programFromSource source = contextual $ do c <- theContext
                                           p <- liftIO $ C.createProgramWithSource c [source]
                                           v <- liftIO $ newMVar False
                                           return (Program p v)

instance Queryable Program ProgramInfo
    where (Program p _) ? info = liftIO $ C.getProgramInfo p info

instance Lifespan Program
    where retain = liftIO . C.retainProgram . program
          release = liftIO . C.releaseProgram . program

buildProgram :: MonadIO m => Program -> [Device] -> String -> m ()
buildProgram (Program p built) devices options = 
     liftIO $ do takeMVar built
                 C.nonBlockingBuildProgram p devices options (putMVar built True)

unloadCompiler :: MonadIO m => m ()
unloadCompiler = liftIO C.unloadCompiler

instance Queryable (Program, Device) ProgramBuildInfo
    where (Program p _, d) ? info = liftIO $ C.getProgramBuildInfo p d info

----------------------------------------------------------------------------------------------------
-- Kernels
----------------------------------------------------------------------------------------------------

-- TODO: threading would be weird with one of these
data Kernel = Kernel { kernelFrom :: Co.Kernel
                     , fixedArgs :: IORef [Int] }

checkBuilt :: MonadIO m => Program -> m ()
checkBuilt p = 
    liftIO $ do isBuilt <- readMVar (built p)
                when (not isBuilt) $ C.userFail "Attempted to extract kernels from unbuilt program" "checkBuilt"

kernel :: MonadIO m => Program -> String -> m Kernel
kernel p name = 
    liftIO $ do checkBuilt p
                fixed <- newIORef ([] :: [Int])
                flip Kernel fixed `fmap` C.createKernel (program p) name

kernels :: MonadIO m => Program -> m [Kernel]
kernels p = liftIO $ do checkBuilt p
                        fixed <- newIORef ([] :: [Int])
                        map (flip Kernel fixed) `fmap` C.createKernelsInProgram (program p)

instance Queryable Kernel KernelInfo
    where k ? info = liftIO $ C.getKernelInfo (kernelFrom k) info

instance Queryable (Kernel, Device) KernelWorkGroupInfo
    where (k, d) ? info = liftIO $ C.getKernelWorkGroupInfo (kernelFrom k) d info

instance Lifespan Kernel
    where retain = liftIO . C.retainKernel . kernelFrom
          release = liftIO . C.releaseKernel . kernelFrom

-- Kernel arguments

class KernelArgument a
    where setArgument :: a -> Co.Kernel -> Int -> IO ()

instance Storable t => KernelArgument t 
    where setArgument v k i = C.setKernelArg k i v

instance Storable t => KernelArgument [t]
    where setArgument vs k i = C.setKernelArgL k i vs

-- Invoking kernels

class KernelInvocation t 
    where invk :: Kernel -> [Co.Kernel -> Int -> IO ()] -> t

invoke :: KernelInvocation r => Kernel -> r
invoke k = invk k [] 

fixArgument :: (KernelArgument a, MonadIO m) => Kernel -> Int -> a -> m ()
fixArgument k i arg = 
    liftIO $ do setArgument arg (kernelFrom k) i
                modifyIORef (fixedArgs k) $ \fixed -> if i `notElem` fixed then i : fixed else fixed

-- Adding arguments
instance (KernelArgument a, KernelInvocation r) => KernelInvocation (a -> r)
    where invk k args a = invk k (setArgument a : args)

-- Invoking

newtype Invocation = Invocation { runI :: IO Co.Kernel }

setArgs :: Kernel -> [Co.Kernel -> Int -> IO ()] -> Invocation
setArgs k args = Invocation $ do fixed <- readIORef (fixedArgs k)
                                 sequence_ [ setArg k' i | setArg <- args | i <- filter (`notElem` fixed) (iterate (subtract 1) (length args + length fixed - 1)) ]
                                 return k'
    where k' = kernelFrom k

instance KernelInvocation Invocation
    where invk k args = setArgs k args

asTask :: Queued m => Invocation -> m Event
asTask kComp = queuedPolite $ \events ->
               do k <- liftIO (runI kComp)
                  q <- ask
                  liftIO $ C.enqueueTask q k events

overRange :: Queued m => Invocation -> ([Int],[Int],[Int]) -> m Event
kComp `overRange` (origin, global, local) = 
    queuedPolite $ \events ->
        do k <- liftIO (runI kComp)
           q <- ask
           liftIO $ C.enqueueNDRangeKernel q k origin global local events
                  

{-

-- Invoke as task
instance Queued m => KernelInvocation (m Event) 
    where invk k args = 
              queuedPolite $ \events ->
                  do q <- ask
                     setArgs k args
                     liftIO $ C.enqueueTask q (kernelFrom k) events

-- Getting around unresolved constraints
type Range = [Int]

nullRange :: Range
nullRange = []

range :: Integral i => [i] -> Range
range = map fromIntegral

noEvents :: [Event]
noEvents = []

-- Invoke as ND range
instance Queued m => KernelInvocation (Range -> Range -> Range -> m Event)
    where invk k args origin global local = 
              queuedPolite $ \events -> 
                  do q <- ask
                     setArgs k args
                     liftIO $ C.enqueueNDRangeKernel q (kernelFrom k) origin global local events


-- Getting around unresolved KernelInvocation constraints
q :: QueueM Event -> QueueM Event
q = id

-}
 
----------------------------------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------------------------------

newEvent :: Contextual m => m Event
newEvent = contextual $ do c <- theContext; liftIO $ C.createUserEvent c

-- This Int parameter sucks
setUserEventStatus :: MonadIO m => Event -> EventStatus -> m ()
setUserEventStatus ev = liftIO . C.setUserEventStatus ev

instance Queryable Event EventInfo
    where e ? info = liftIO $ C.getEventInfo e info

instance Lifespan Event
    where retain = liftIO . C.retainEvent
          release = liftIO . C.releaseEvent

setEventCallback :: (Storable a, MonadIO m) => Event -> EventCallback a -> a -> m ()
setEventCallback ev callback value = liftIO $ C.setEventCallback ev callback value

setDatalessEventCallback :: MonadIO m => Event -> EventCallback a -> m ()
setDatalessEventCallback ev callback = liftIO $ C.setDatalessEventCallback ev callback

waitForEvents :: Queued m => [Event] -> m ()
waitForEvents events = do queue <- theQueue; liftIO $ C.enqueueWaitForEvents queue events

blockWaitingForEvents :: MonadIO m => [Event] -> m ()
blockWaitingForEvents events = liftIO $ C.waitForEvents events

----------------------------------------------------------------------------------------------------
-- Flush and finish
----------------------------------------------------------------------------------------------------

flush, finish :: Queued m => m ()
flush = queued $ theQueue >>= liftIO . C.flush
finish = queued $ theQueue >>= liftIO . C.finish