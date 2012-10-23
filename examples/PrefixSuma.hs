{-# LANGUAGE ScopedTypeVariables #-}

module PrefixSumA where

import Language.OpenCL.Host
import Control.Monad.Trans (liftIO)

main = do [p] <- platforms
          [gpu] <- devicesOfType p [GPU]
          withNew (context p [gpu]) $ 
              using (programFromSource =<< liftIO (readFile "prefixSum_kernels.cl")) $ \ p -> 
              using (buffer 5) $ \ inBuf ->
              using (buffer 5) $ \ outBuf -> 
                  do { buildProgram p [gpu] ""
                     ; using (kernel p "square") $ \ square -> 
                       using (kernel p "prefixSumStep") $ \ prefixSumStep ->
                       withNew (queue gpu) $ 
                           do writeTo inBuf 0 [1,2,3,4,5::Int] 
                              invoke square inBuf `overRange` ([0], [5], [1])
                              flip mapM_ [0..3::Int] $ \ iteration ->
                                  do invoke prefixSumStep iteration inBuf outBuf `overRange` ([0], [5], [1]) 
                                     copyBuffer outBuf inBuf 0 0 5 
                              readFrom inBuf 0 5 >>= \(x :: [Int]) -> liftIO $ print x }
