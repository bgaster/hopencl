{-# LANGUAGE DeriveDataTypeable #-}
module CLUtil (
  clInit,
  CLEnv(..))
       where

import Language.OpenCL.Host.Constants
import Language.OpenCL.Host.Core

import ParserQuote

data CLEnv = CLEnv Platform Context Program CommandQueue [Kernel]
     
clInit :: CLProg -> IO CLEnv
clInit (CLProg src dtype) =
  do (_:p:_) <- getPlatforms
     putStrLn . ("Platform is by: " ++) =<< getPlatformInfo p PlatformVendor
     c <- createContextFromType (pushContextProperty ContextPlatform p noProperties) (bitSet [dtype])
     prog <- createProgramWithSource c ([src])
     ds <- getContextInfo c ContextDevices
     putStrLn "Building.."
     buildProgram prog ds ""
     putStrLn "Done!"
     k <- createKernel prog "hello"
     q <- createCommandQueue c (head ds) (bitSet [])
     return $ CLEnv p c prog q [k]
