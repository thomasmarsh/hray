{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

-- Some imports

import           Control.Monad               ( replicateM )
import           Control.Monad.Random        ( Rand
                                             , RandomGen
                                             , StdGen
                                             , evalRand
                                             , getRandom
                                             )
import           Data.Maybe                  ( fromJust, isNothing )
import           Linear.V3                   ( V3(..) )
import           System.IO                   ( hClose
                                             , openFile
                                             , IOMode (WriteMode)
                                             )
import           System.Random               ( newStdGen )
import qualified Text.Printf                 as P

import Debug.Trace (traceM)

-- from numeric-limits
maxValue :: (RealFloat a) => a
maxValue = x
  where n = floatDigits x
        b = floatRadix x
        (_, u) = floatRange x
        x = encodeFloat (b^n - 1) (u - n)

-- Some types

type    F3          = V3        Float
newtype Color       = Color     F3
newtype Color3i     = Color3i   (Int, Int, Int)
newtype Dimensions  = Dim       (Int, Int)
newtype Coord       = Coord     (Int, Int)

newtype Normal      = Normal F3
newtype Point       = Point  F3

data Ray = Ray !F3 -- origin
               !F3 -- direction
        deriving Show

data HitRecord g
    = HitRecord
        !Float  -- t
        !Point  -- p
        !Normal -- normal
        (ScatterF g)

mkHitRecord :: RandomGen g => Float -> Point -> Normal -> HitRecord g
mkHitRecord t p normal = HitRecord t p normal nullScatter

data Camera = Camera
    { lowerLeft  :: !F3
    , horizontal :: !F3
    , vertical   :: !F3
    , camOrigin  :: !F3
    }

data Config g = Config
    { screenSize :: Dimensions
    , camera     :: Camera
    , scene      :: [Object g]
    , nSamples   :: Int
    }

-- Hitable

type HitF g = Ray -> (Float, Float) -> Maybe (HitRecord g)

-- Material

type ScatterF g =
    RandomGen g
    => Ray                      -- input ray
    -> Point                    -- p
    -> Normal                   -- normal
    -> Rand g (Maybe (Ray, F3)) -- Just (r, attenuation) if scattered

nullScatter :: RandomGen g => ScatterF g
nullScatter _ _ _ = pure Nothing

lambertian :: RandomGen g => F3 -> ScatterF g
lambertian albedo _ (Point p) (Normal normal) = do
    rSphere <- randomInUnitSphere
    let target = p + normal + rSphere
    let scattered = Ray p (target - p)
    pure $ Just (scattered, albedo)

metal :: RandomGen g => F3 -> ScatterF g
metal albedo (Ray _ direction) (Point p) (Normal normal) = do
    let reflected = reflect (unitVec direction) normal
    let scattered = Ray p reflected
    pure $ if reflected `dot` normal < 0
                then Just (scattered, albedo)
                else Nothing

data Object g = Object
    { hitF     :: HitF g
    , scatterF :: ScatterF g
    }

newtype Sphere = Sphere (F3, Float)

hitSphere :: RandomGen g => Sphere -> HitF g
hitSphere (Sphere (center, radius)) r@(Ray origin direction) (tMin, tMax)
    | hasHit    = Just hr'
    | otherwise = Nothing
    where
        oc           = origin - center
        a            = direction `dot` direction
        b            = oc `dot` direction
        c            = (oc `dot` oc) - radius * radius
        discriminant = b*b - a*c
        op | discriminant > 0 = (-)
           | otherwise        = (+)
        intersect  = (-b `op` sqrt (b*b-a*c)) / a
        hasHit     = intersect > tMin && intersect < tMax
        pAt        = pointAtParameter r intersect
        hr'        = mkHitRecord intersect
                                 (Point pAt)
                                 (Normal ((pAt - center) / mkV radius))

setScatterF :: RandomGen g => ScatterF g -> HitRecord g -> HitRecord g
setScatterF f (HitRecord t p n _) = HitRecord t p n f

hitList :: RandomGen g => [Object g] -> HitF g
hitList os r (tMin, tMax)
    = go tMax Nothing os
    where
        go :: RandomGen g => Float -> Maybe (HitRecord g) -> [Object g] -> Maybe (HitRecord g)
        go _       hr [] = hr
        go closest hr (o:os')
            = case (hitF o) r (tMin, closest) of
                Just hr'@(HitRecord t _ _ _)
                        -> go t (Just $ setScatterF (scatterF o) hr') os'
                Nothing -> go closest hr  os'

sphere :: RandomGen g => F3 -> Float -> ScatterF g -> Object g
sphere pos radius m = Object
    { hitF     = hitSphere (Sphere (pos, radius))
    , scatterF = m
    }

-- Vector utils

mkV :: a -> V3 a
mkV t = V3 t t t

mag :: F3 -> Float
mag = sqrt . squaredLength

unitVec :: F3 -> F3
unitVec v = v / (mkV $ mag v)

dot :: F3 -> F3 -> Float
dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

squaredLength :: F3 -> Float
squaredLength (V3 x y z) = x*x + y*y + z*z

pointAtParameter :: Ray -> Float -> F3
pointAtParameter (Ray a b) t = a + b*(mkV t)

reflect :: F3 -> F3 -> F3
reflect v n = v - mkV (2 * v `dot` n) * n

-- Background

color :: RandomGen g
      => [Object g]
      -> Int
      -> Ray
      -> Rand g Color
color world depth r@(Ray _ direction)
    = case hitList world r (epsilon, maxValue::Float) of
        Just (HitRecord _ p normal scatter) -> do
            scattered <- scatter r p normal
            if depth >= 50 || isNothing scattered
                then pure $ Color $ mkV 0
                else do
                    let (r', attenuation) = fromJust scattered
                    (Color bounce) <- color world (depth+1) r'
                    pure $ Color $ attenuation * bounce
        Nothing ->
            pure $ Color $ (one-t)*one + t*(V3 0.5 0.7 1.0)
    where
        (V3 _ y _) = unitVec direction
        t          = mkV $ 0.5 * (y + 1.0)
        one        = mkV 1.0
        epsilon    = 0.001   -- shadow acne correction

-- https://karthikkaranth.me/blog/generating-random-points-in-a-sphere/
randomInUnitSphere'ClosedForm :: RandomGen g => Rand g F3
randomInUnitSphere'ClosedForm = do
    u <- getRandom
    v <- getRandom
    let theta = u * 2 * pi
    let phi = acos $ 2 * v - 1
    ri <- getRandom
    let r = ri ** (1/3)
    let sinPhi = sin phi
    let x = r * sinPhi * cos theta
    let y = r * sinPhi * sin theta
    let z = r * cos phi
    pure $ V3 x y z

randomInUnitSphere :: RandomGen g => Rand g F3
randomInUnitSphere = randomInUnitSphere'ClosedForm

-- Core render loop

mkRay :: Camera -> (F3, F3) -> Ray
mkRay cam (u, v) =
    Ray (camOrigin cam)
        (lowerLeft cam + u * (horizontal cam) + v * (vertical cam))

coords :: Dimensions -> [Coord]
coords (Dim (nx, ny)) = [Coord (i, j) | j <- [ny-1,ny-2..0], i <- [0..nx-1]]

coordToUV :: RandomGen g => Dimensions -> Coord -> Rand g (F3, F3)
coordToUV (Dim (nx, ny)) (Coord (i, j)) = do
    rx <- getRandom
    ry <- getRandom
    pure $ (,) (scale i nx rx)
               (scale j ny ry)
    where
        scale k z r = mkV $ (r + fromIntegral k) / fromIntegral z

-- Scale [0..1) to [0..255], with gamma correction
scaleColor :: Color -> Color3i
scaleColor (Color (V3 r g b)) = Color3i (f r, f g, f b)
    where f c = floor (255.9 * sqrt c) -- sqrt for gamma correction

-- Find the mean of a list of colors
avgColor :: Int -> [Color] -> Color
avgColor n cs = Color $ sum (map (\(Color c) -> c) cs) / mkV (fromIntegral n)

-- Given an 
antialias :: RandomGen g
          => Config g
          -> ((F3, F3) -> Rand g Color)
          -> Coord
          -> Rand g Color3i
antialias cfg f coord = do
    replicateM (nSamples cfg) (coordToUV (screenSize cfg) coord)
        >>= mapM f
        >>= pure . scaleColor
                 . avgColor (nSamples cfg)
        
render :: RandomGen g => Config g -> Rand g [Color3i]
render cfg
    = mapM (antialias cfg sample)
           (coords $ screenSize cfg)
    where sample = color (scene cfg) 0
                 . mkRay (camera cfg)

writePPM :: FilePath -> Dimensions -> [Color3i] -> IO ()
writePPM path (Dim (nx, ny)) cs = do
    h <- openFile path WriteMode
    P.hPrintf h "P3\n%d %d\n255\n" nx ny
    mapM_ (\(Color3i (r, g, b))
            -> P.hPrintf h "%d %d %d\n" r g b) cs
    hClose h

-- Scene

gConfig :: RandomGen g => Config g
gConfig = Config 
    { screenSize = Dim (400, 200)
    , camera     = Camera
                   { lowerLeft  = V3 (-2) (-1) (-1)
                   , horizontal = V3 4 0 0
                   , vertical   = V3 0 2 0
                   , camOrigin  = V3 0 0 0
                   }
    , scene      = [ sphere (V3 0 0 (-1))
                            0.5
                            (lambertian (V3 0.8 0.3 0.3))
                   , sphere (V3 0 (-100.5) (-1))
                            100
                            (lambertian (V3 0.8 0.8 0.0))
                   , sphere (V3 1 0 (-1))
                            0.5
                            (metal (V3 0.8 0.6 0.2))
                   , sphere (V3 (-1) 0 (-1))
                            0.5
                            (metal (V3 0.8 0.8 0.8))
                   ]
    , nSamples   = 100
    }

main :: IO ()
main = do
    g <- newStdGen
    let cs = evalRand (render gConfig) g
    writePPM "test.ppm" (screenSize (gConfig :: Config StdGen)) cs