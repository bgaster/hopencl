{-# OPTIONS_GHC -XQuasiQuotes #-}
module Tutorial1 where

import Foreign.C

import Language.OpenCL.Module

--------------------------------------------------------------------------------

hwlen = length "Hello world\n" + 1

prog = initCL [$clCPU|
  #pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable
  __constant char hw[] = "Hello World\n";
  __kernel void hello(__global char * out) {
	size_t tid = get_global_id(0);
	out[tid] = hw[tid];
  }  
|]

main :: IO ()
main = withNew prog $ 
         using (bufferWithFlags hwlen [WriteOnly]) $ \b ->
           do [k] <- theKernels
              invoke k b `overRange` ([0], [hwlen], [1])
              liftIO . putStr . map castCCharToChar . fst =<< readBuffer b 0 (hwlen - 1)
