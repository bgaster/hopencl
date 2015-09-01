{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module Language.OpenCL.Module (
    module Language.OpenCL.Host
--  , CLModule(..)
--  , initCL
--  , clCPU
  , Module(..) 
  , ModuleM(..)
  , theKernels) where
 
import Data.Generics
import qualified Language.Haskell.TH as TH

--import Language.Haskell.TH.Quote
 
import Text.ParserCombinators.Parsec

import Language.OpenCL.Host
import Language.OpenCL.Host.Constants hiding (Kernel, Program, size)
import qualified Language.OpenCL.Host.Constants
import Language.OpenCL.Host.Core

import Language.OpenCL.Host as CL
import Language.OpenCL.Host.FFI

import Control.Monad.Reader

import System.IO.Unsafe

----------------------------------------------------------------------------------------------------
-- Modules
----------------------------------------------------------------------------------------------------

data Module = Mod { context_ ::  Context 
                  , kernels_ :: [Kernel] 
                  , queue_ :: CommandQueue }

newtype ModuleM t = ModuleM { runModuleM :: PoliteT (ReaderT Module IO) t } 
    deriving (Functor, Applicative, Monad, MonadIO, CatchIO, MonadReader Module)

instance Lifespan Module
    where retain (Mod c ks q) = sequence_ [ retain c, retain ks, retain q ]
          release (Mod c ks q) = sequence_ [ release c, release ks, release q ]

instance Contextual ModuleM 
    where contextual (ContextM c) = 
            ModuleM (preservingPoliteness (\c' -> asks context_ >>= liftIO . runReaderT c') c)

instance Queued ModuleM
    where queued (QueueM c) = 
            ModuleM (preservingPoliteness (\c' -> do ctxt <- asks context_
                                                     q <- asks queue_
                                                     liftIO (runReaderT (runReaderT c' q) ctxt)) c)

instance MonadIO m => Wraps Module ModuleM m
    where with mod comp = liftIO (runReaderT (runPolite (runModuleM comp)) mod)
     
theKernels :: ModuleM [Kernel]
theKernels = asks kernels_

----------------------------------------------------------------------------------------------------
-- QuasiQuoter
----------------------------------------------------------------------------------------------------

{-

data CLModule = CLModule String DeviceType
    deriving(Show, Typeable, Data)
                                             
parseCLProg :: Monad m => (String, Int, Int) -> DeviceType -> String -> m CLModule
parseCLProg pos dtype s = return (unsafePerformIO (parseCLProg' pos dtype s))

parseCLProg' :: (String, Int, Int) -> DeviceType -> String -> IO CLModule
parseCLProg' (file, line, col) dtype s = 
  do (_:p:_) <- getPlatforms
     putStrLn . ("Platform is by: " ++) =<< getPlatformInfo p PlatformVendor
     c <- createContextFromType 
          (pushContextProperty ContextPlatform p noProperties) (bitSet [dtype])
     putStrLn "hello"
     p <- createProgramWithSource c [s]
     ds <- getContextInfo c ContextDevices
     putStrLn "Building.."
     Language.OpenCL.Host.Core.buildProgram p ds ""
     putStrLn "Done!"
     return $ CLModule s dtype
   
----------------------------------------------------------------------------------------------------

quoteCLProgExp :: DeviceType -> String -> TH.ExpQ
quoteCLProgPat :: DeviceType -> String -> TH.PatQ
 
clCPU  :: QuasiQuoter
clCPU  =  QuasiQuoter (quoteCLProgExp CPU) (quoteCLProgPat CPU)

quoteCLProgExp dtype s =  
  do  loc <- TH.location
      let pos =  (TH.loc_filename loc,
                  fst (TH.loc_start loc),
                  snd (TH.loc_start loc))
      prog <- parseCLProg pos dtype s 
      dataToExpQ (const Nothing) prog
                        
quoteCLProgPat dtype s =  
  do  loc <- TH.location
      let pos =  (TH.loc_filename loc,
                  fst (TH.loc_start loc),
                  snd (TH.loc_start loc))
      prog <- parseCLProg pos dtype s
      dataToPatQ (const Nothing) prog

----------------------------------------------------------------------------------------------------
-- Utilities stuff for modules
----------------------------------------------------------------------------------------------------

initCL :: CatchIO m => CLModule -> m Module
initCL (CLModule src dtype) =
  do (_:p:_) <- CL.platforms
     withNew (CL.contextFromType p [dtype]) $
        using (CL.programFromSource src) $ \prog -> 
        do c <- CL.theContext
           ds <- CL.queryContext ContextDevices
           CL.buildProgram prog ds ""
           ks <- kernels prog
           q <- queue (head ds)
           return (Mod c ks q)

-}
