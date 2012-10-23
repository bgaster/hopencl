{-# LANGUAGE ScopedTypeVariables #-}

module Tutorial1b where

import Language.OpenCL.Host
import Language.OpenCL.Host.FFI

import Control.Monad.Trans (liftIO)
import Data.Array.Storable hiding (range)
import Foreign.C (castCCharToChar)

hwlen = length "Hello world\n" + 1

main = do [p] <- platforms
          putStrLn . ("Platform is by: " ++) =<< p ? PlatformVendor
          withNew (contextFromType p [CPU]) $
              using (programFromSource =<< liftIO (readFile "tutorial1_kernel.cl")) $ \ p -> 
              do ds <- queryContext ContextDevices
                 buildProgram p ds ""
                 using (bufferWithFlags hwlen [WriteOnly]) $ \ b ->
                     using (kernel p "hello") $ \ k -> 
                     withNew (queue (head ds)) $ 
                         do invoke k b `overRange` ([0], [hwlen], [1])                                                         
                            liftIO . putStr . map castCCharToChar =<< readFrom b 0 (hwlen - 1) 