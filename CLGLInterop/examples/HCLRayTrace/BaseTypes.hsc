{-# OPTIONS_GHC -cpp #-}
module BaseTypes where

import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Language.OpenCL.Host

#include "BaseTypes.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

----------------------------------------------------------------------------------------------------
-- Types passed to CL

data Vector = Vec !Float !Float !Float deriving (Eq, Show)

instance Storable Vector
    where sizeOf _    = #{size Vec}
          alignment _ = #{alignment Vec}
          peek ptr = 
              do x <- #{peek Vec, x} ptr
                 y <- #{peek Vec, y} ptr
                 z <- #{peek Vec, z} ptr
                 return (Vec x y z)
          poke ptr (Vec x y z) =
               do #{poke Vec, x} ptr x
                  #{poke Vec, y} ptr y
                  #{poke Vec, z} ptr z

data Ray = Ray !Vector !Vector deriving (Show)

instance Storable Ray
    where sizeOf _    = #{size Ray}
          alignment _ = #{alignment Ray}
          peek ptr =
              do origin <- #{peek Ray, o} ptr
                 direction <- #{peek Ray, d} ptr
                 return (Ray origin direction)
          poke ptr (Ray origin direction) =
              do #{poke Ray, o} ptr origin
                 #{poke Ray, d} ptr direction

data Camera = Camera { origin :: !Vector
                     , target :: !Vector 
                     , dir, x, y :: !Vector } deriving (Show)

instance Storable Camera
    where sizeOf _    = #{size Camera}
          alignment _ = #{alignment Camera}
          peek ptr =
              do origin <- #{peek Camera, orig} ptr
                 target <- #{peek Camera, target} ptr
                 dir <- #{peek Camera, dir} ptr
                 x <- #{peek Camera, x} ptr
                 y <- #{peek Camera, y} ptr
                 return (Camera origin target dir x y)
          poke ptr (Camera origin target dir x y) = 
              do #{poke Camera, orig} ptr origin
                 #{poke Camera, target} ptr target
                 #{poke Camera, dir} ptr dir
                 #{poke Camera, x} ptr x
                 #{poke Camera, y} ptr y

data Reflection = Diffuse | Specular | Refractive deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Storable Reflection
    where sizeOf x    = sizeOf (fromEnum x)
          alignment x = alignment (fromEnum x)
          peek ptr    = toEnum `fmap` peek (castPtr ptr)
          poke ptr    = poke (castPtr ptr) . fromEnum

data Sphere = Sphere { radius :: !Float
                     , position, emission, color :: !Vector
                     , reflection :: !Reflection } deriving (Show)

instance Storable Sphere
    where sizeOf _    = #{size Sphere}
          alignment _ = #{alignment Sphere}
          peek ptr =
              do radius <- #{peek Sphere, rad} ptr
                 position <- #{peek Sphere, p} ptr
                 emission <- #{peek Sphere, e} ptr
                 color <- #{peek Sphere, c} ptr
                 reflection <- #{peek Sphere, refl} ptr
                 return (Sphere radius position emission color reflection)
          poke ptr (Sphere radius position emission color reflection) = 
              do #{poke Sphere, rad} ptr radius
                 #{poke Sphere, p} ptr position
                 #{poke Sphere, e} ptr emission
                 #{poke Sphere, c} ptr color
                 #{poke Sphere, refl} ptr reflection

----------------------------------------------------------------------------------------------------
-- Haskell-only types

data Scene t = Scene { width, height :: Word
                     , sphereCount :: Word
                     , spheres :: t } deriving (Show)

data Buffers = Buffers { colors :: Buffer Vector
                       , pixels :: Buffer Word }

instance Lifespan Buffers
    where retain (Buffers colors pixels) = retain colors >> retain pixels
          release (Buffers colors pixels) = release colors >> release pixels

----------------------------------------------------------------------------------------------------
-- Vector operations

(Vec x0 y0 z0) `dot` (Vec x1 y1 z1) = x0 * x1 + y0 * y1 + z0 * z1
smult s (Vec x y z) = Vec (s * x) (s * y) (s * z)
normalize v = smult l v
    where l = 1 / sqrt (v `dot` v)
(Vec x0 y0 z0) `cross` (Vec x1 y1 z1) = Vec (y0 * z1 - z0 * y1) (z0 * x1 - x0 * z1) (x0 * y1 - y0 * x1)
