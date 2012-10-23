{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC  -XMultiParamTypeClasses #-}
module Module (Module(..), ModuleM(..), theKernels) where

import Control.Monad.Reader
import Language.OpenCL.Host

data Module = Mod { context_ ::  Context 
                  , kernels_ :: [Kernel] 
                  , queue_ :: CommandQueue }

newtype ModuleM t = ModuleM { runModuleM :: PoliteT (ReaderT Module IO) t } 
    deriving (Functor, Monad, MonadIO, CatchIO, MonadReader Module)

instance Lifespan Module
    where retain (Mod c ks q) = sequence_ [ retain c, retain ks, retain q ]
          release (Mod c ks q) = sequence_ [ release c, release ks, release q ]

instance Contextual ModuleM 
    where contextual (ContextM c) = ModuleM (preservingPoliteness (\c' -> asks context_ >>= liftIO . runReaderT c') c)

instance Queued ModuleM
    where queued (QueueM c) = ModuleM (preservingPoliteness (\c' -> do ctxt <- asks context_
                                                                       q <- asks queue_
                                                                       liftIO (runReaderT (runReaderT c' q) ctxt)) c)

instance Wraps Module ModuleM IO 
    where with mod comp = runReaderT (runPolite (runModuleM comp)) mod
     
theKernels :: ModuleM [Kernel]
theKernels = asks kernels_
