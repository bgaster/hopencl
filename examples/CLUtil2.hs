{-# OPTIONS_GHC -XFlexibleContexts #-}

{-# LANGUAGE DeriveDataTypeable #-}
module CLUtil2 (
  clInit2,
  CLEnv2(..))
       where

import Language.OpenCL.Host
import Language.OpenCL.Host.FFI

import Control.Monad.Trans (liftIO, lift)
import Control.Monad (join)
import Foreign.C (castCCharToChar)

import Module
import ParserQuote

data CLEnv2 = CLEnv2 [Kernel]
     
clInit2 :: CatchIO m => CLProg -> m Module
clInit2 (CLProg src dtype) =
  do (_:p:_) <- platforms
     withNew (contextFromType p [dtype]) $
        using (programFromSource src) $ \prog -> 
        do c <- theContext
           ds <- queryContext ContextDevices
           buildProgram prog ds ""
           ks <- kernels prog
           q <- queue (head ds)
           return (Mod c ks q)

hwlen = length "Hello world\n" + 1

{-
main' prog = do using (clInit2 prog) $ \k' ->
                  using (bufferWithFlags hwlen [WriteOnly]) $ \b ->
                  do k <- k'
                     invoke k b `overRange` ([0], [hwlen], [1])
                     liftIO . putStr . map castCCharToChar . fst =<< readBuffer b 0 (hwlen - 1)
                     release k
 -}

--main'' :: CLProg -> IO ()

{-
aux :: MonadIO m => m ()
aux = return ()

instance Lifespan ()
    where retain = liftIO . return ()
          release = liftIO . return ()
-}

main'' :: CLProg -> IO ()
main'' prog = withNew (clInit2 prog) $ 
                 using (bufferWithFlags hwlen [WriteOnly]) $ \b ->
                     do [k] <- theKernels
                        invoke k b `overRange` ([0], [hwlen], [1])
                        liftIO . putStr . map castCCharToChar . fst =<< readBuffer b 0 (hwlen - 1)