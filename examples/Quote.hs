{-# LANGUAGE DeriveDataTypeable #-}
module Quote (
  CLModule(..),
  initCL,
  clCPU) where
 
import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
 
import Text.ParserCombinators.Parsec

import Language.OpenCL.Host.Constants
import Language.OpenCL.Host.Core

import Language.OpenCL.Host as CL
import Language.OpenCL.Host.FFI

import System.IO.Unsafe

--------------------------------------------------------------------------------

data CLModule = CLModule String DeviceType
    deriving(Show, Typeable, Data)
                                  
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
           
--------------------------------------------------------------------------------

parseCLProg :: Monad m => (String, Int, Int) -> DeviceType -> String -> m CLModule
parseCLProg pos dtype s = return (unsafePerformIO (parseCLProg' pos dtype s))

parseCLProg' :: (String, Int, Int) -> DeviceType -> String -> IO CLModule
parseCLProg' (file, line, col) dtype s = 
  do (_:p:_) <- getPlatforms
     putStrLn . ("Platform is by: " ++) =<< getPlatformInfo p PlatformVendor
     c <- createContextFromType 
          (pushContextProperty ContextPlatform p noProperties) (bitSet [dtype])
     p <- createProgramWithSource c [s]
     ds <- getContextInfo c ContextDevices
     putStrLn "Building.."
     Language.OpenCL.Host.Core.buildProgram p ds ""
     putStrLn "Done!"
     return $ CLModule s dtype
   
--------------------------------------------------------------------------------

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
