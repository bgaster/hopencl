{-# OPTIONS_GHC -XNamedFieldPuns #-}

{-
Copyright ©2012 Advanced Micro Devices, Inc. All rights reserved.

*   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
*   Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.  
*   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
********************************************************************

-}

module Scene where

import BaseTypes
import Control.Monad (when)

zeroVec = Vec 0 0 0

for :: [a] -> (a -> b) -> [b]
for = flip map

norm cs@('.':_) = '0':cs
norm cs         = cs

readScene :: Int -> Int -> String -> IO (Camera, [Sphere])
readScene width height fname = 
    do ls <- lines `fmap` readFile fname
       -- read the camera position
       let camera = case words (ls !! 0) of
                      ("camera" : rest) -> 
                          let [origX, origY, origZ, targetX, targetY, targetZ] = map read rest
                          in computeCamera width height (Vec origX origY origZ) (Vec targetX targetY targetZ)
                      _ -> error "failed to parse camera"
           sphereCount = case words (ls !! 1) of
                           ("size" : rest) ->
                               let [sphereCount] = map read rest in sphereCount
                           _ -> error "failed to read sphere count"
           spheres = for (tail (tail ls)) $
                     \ line -> case words line of
                                 ["sphere", srad, spx, spy, spz, sex, sey, sez, scx, scy, scz, smat] ->
                                     let [rad, px, py, pz, ex, ey, ez, cx, cy, cz] = map read [norm srad, norm spx, norm spy, norm spz, norm sex, norm sey, norm sez, norm scx, norm scy, norm scz]
                                         mat = read smat
                                     in Sphere { radius = rad
                                               , position = Vec px py pz
                                               , emission = Vec ex ey ez
                                               , color = Vec cx cy cz
                                               , reflection = toEnum mat }
                                 _ -> error $ "failed to parse sphere " ++ line
       when (length spheres /= sphereCount) (putStrLn $ "Expected " ++ show sphereCount ++ " spheres but found " ++ show (length spheres) ++ " instead.")
       return (camera, spheres)
                                         

computeCamera :: Int -> Int -> Vector -> Vector -> Camera
computeCamera iWidth iHeight origin@(Vec ox oy oz) target@(Vec tx ty tz) = 
    Camera { origin, target, dir, x, y }
    where width  = fromIntegral iWidth
          height = fromIntegral iHeight
          up     = Vec 0 1 0
          fov    = pi / 180 * 45          

          dir    =  normalize (Vec (tx - ox) (ty - oy) (tz - oz))
          x      = (width * fov / height) `smult` normalize (dir `cross` up)
          y      = fov `smult` normalize (x `cross` dir)
          