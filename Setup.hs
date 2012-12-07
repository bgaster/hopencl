import System.Info
import Data.List
import Distribution.Simple
import Distribution.Simple.Setup

-- Inspired by the Setup.hs file included in llvm-core

main = defaultMainWithHooks hooks
    where hooks | os == "mingw32" = autoconfUserHooks { postConf = generateBuildInfo }
                | otherwise       = simpleUserHooks

generateBuildInfo _ conf _ _ =
    do info <- readFile "HOpenCL.buildinfo.in"
       writeFile "HOpenCL.buildinfo" $ subst "@opencl_path@" (subst "\\" "\\\\" path) info
    where path = case [ p | arg <- configConfigureArgs conf, Just p <- [stripPrefix "--with-opencl-prefix=" arg] ] of
                   [p] -> p
                   _ -> error $ "Use '--configure-option --with-opencl-prefix=PATH' to give OpenCL installation path"

subst from to []                                 = []
subst from to xs | Just r <- stripPrefix from xs = to ++ subst from to r
subst from to (x:xs)                             = x : subst from to xs
