{-# LANGUAGE DeriveDataTypeable #-}
module ParserQuote (
  CLProg(..),
  parseCLProg,
  initCL)
       where
 
import Data.Generics
import Text.ParserCombinators.Parsec

import Language.OpenCL.Host.Constants
import Language.OpenCL.Host.Core

import Language.OpenCL.Host as CL
import Language.OpenCL.Host.FFI

import Module

import System.IO.Unsafe

--------------------------------------------------------------------------------

data CLProg = CLProg String DeviceType
    deriving(Show, Typeable, Data)
            
parseCLProg :: Monad m => (String, Int, Int) -> DeviceType -> String -> m CLProg
parseCLProg pos dtype s = return (unsafePerformIO (parseCLProg' pos dtype s))

{-
parseCLProg' :: CatchIO m => (String, Int, Int) -> DeviceType -> String -> m Module
parseCLProg' (file, line, col) dtype src = 
  do (_:p:_) <- platforms
     withNew (contextFromType p [dtype]) $
        using (programFromSource src) $ \prog -> 
        do c <- theContext
           ds <- queryContext ContextDevices
           buildProgram prog ds ""
           ks <- kernels prog
           q <- queue (head ds)
           return (Mod c ks q)
-}

parseCLProg' :: (String, Int, Int) -> DeviceType -> String -> IO CLProg
parseCLProg' (file, line, col) dtype s = 
  do (_:p:_) <- getPlatforms
     putStrLn . ("Platform is by: " ++) =<< getPlatformInfo p PlatformVendor
     c <- createContextFromType (pushContextProperty ContextPlatform p noProperties) (bitSet [dtype])
     putStrLn "hello"
     p <- createProgramWithSource c [s]
     putStrLn "hello"
     ds <- getContextInfo c ContextDevices
     putStrLn "Building.."
     Language.OpenCL.Host.Core.buildProgram p ds ""
     putStrLn "Done!"
     return $ CLProg s dtype
   
--------------------------------------------------------------------------------
                      
initCL :: CatchIO m => CLProg -> m Module
initCL (CLProg src dtype) =
  do (_:p:_) <- CL.platforms
     withNew (CL.contextFromType p [dtype]) $
        using (CL.programFromSource src) $ \prog -> 
        do c <- CL.theContext
           ds <- CL.queryContext ContextDevices
           CL.buildProgram prog ds ""
           ks <- kernels prog
           q <- queue (head ds)
           return (Mod c ks q)