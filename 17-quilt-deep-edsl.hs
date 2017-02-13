{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

import           Codec.Picture
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.Word

type Color  = Colour Double
type Number = Double

data Coord where
  X :: Coord
  Y :: Coord

data Quilt a where
  QSolid :: a -> Quilt a
  QCoord :: Coord -> Quilt Number
  QGrey  :: Quilt Number -> Quilt Color
  QIf    :: Quilt Bool -> Quilt a -> Quilt a -> Quilt a
  QQuilt :: Quilt a -> Quilt a -> Quilt a -> Quilt a -> Quilt a
  QMap   :: (a -> b) -> Quilt a -> Quilt b
  QZip   :: (a -> b -> c) -> Quilt a -> Quilt b -> Quilt c
  QRot   :: Quilt a -> Number -> Quilt a

quilt :: Quilt a -> Quilt a -> Quilt a -> Quilt a -> Quilt a
quilt = QQuilt

solid :: a -> Quilt a
solid = QSolid

x :: Quilt Number
x = QCoord X

y :: Quilt Number
y = QCoord Y

mkGrey :: Quilt Number -> Quilt Color
mkGrey = QGrey

ifQ :: Quilt Bool -> Quilt a -> Quilt a -> Quilt a
ifQ = QIf

(<.) :: Ord a => Quilt a -> Quilt a -> Quilt Bool
(<.) = QZip (<)

mapQuilt :: (a -> b) -> Quilt a -> Quilt b
mapQuilt = QMap

zipQuilt :: (a -> b -> c) -> Quilt a -> Quilt b -> Quilt c
zipQuilt = QZip

rot :: Quilt a -> Number -> Quilt a
rot = QRot

mapColor :: (Double -> Double) -> Color -> Color
mapColor f (toSRGB -> RGB r g b) = sRGB (f r) (f g) (f b)

zipColor :: (Double -> Double -> Double) -> Color -> Color -> Color
zipColor (&) (toSRGB -> RGB r1 g1 b1) (toSRGB -> RGB r2 g2 b2) = sRGB (r1 & r2) (g1 & g2) (b1 & b2)

instance Num Color where
  (+) = zipColor (+)
  (-) = zipColor (-)
  (*) = zipColor (*)
  abs = mapColor abs
  signum = mapColor signum
  fromInteger i = sRGB i' i' i'
    where i' = fromInteger i

instance Num a => Num (Quilt a) where
  (+)           = zipQuilt (+)
  (-)           = zipQuilt (-)
  (*)           = zipQuilt (*)
  abs           = mapQuilt abs
  signum        = mapQuilt signum
  fromInteger i = solid (fromInteger i)

instance Fractional a => Fractional (Quilt a) where
  fromRational = solid . fromRational
  (/) = zipQuilt (/)

instance Floating a => Floating (Quilt a) where
  pi    = solid pi
  exp   = mapQuilt exp
  log   = mapQuilt log
  sin   = mapQuilt sin
  cos   = mapQuilt cos
  asin  = mapQuilt asin
  acos  = mapQuilt acos
  atan  = mapQuilt atan
  sinh  = mapQuilt sinh
  cosh  = mapQuilt cosh
  asinh = mapQuilt asinh
  acosh = mapQuilt acosh
  atanh = mapQuilt atanh

type QuiltFun a = Double -> Double -> a

interp :: Quilt a -> QuiltFun a
interp (QSolid c) = \_ _ -> c
interp (QCoord X) = \x _ -> x
interp (QCoord Y) = \_ y -> y
interp (QGrey c) = \x y -> let z = interp c x y in sRGB z z z
interp (QIf cond eT eF) = \x y -> if interp cond x y then interp eT x y
                                                     else interp eF x y
interp (QQuilt q1 q2 q3 q4) = \x y ->
  case (x < 0, y > 0) of
    (True , True)  -> interp q1 (2*x + 1) (2*y - 1)
    (True , False) -> interp q3 (2*x + 1) (2*y + 1)
    (False, True)  -> interp q2 (2*x - 1) (2*y - 1)
    (False, False) -> interp q4 (2*x - 1) (2*y + 1)
interp (QMap f q) = \x y -> f (interp q x y)
interp (QZip f q1 q2) = \x y -> f (interp q1 x y) (interp q2 x y)
interp (QRot q deg) = \x y ->
    let rad' = pi / 180 * deg
    in interp q (x * cos rad' - y * sin rad') (x * sin rad' + y * cos rad')

quilterate :: Int -> Quilt a -> Quilt a
quilterate 0 q = q
quilterate n q = let q' = quilterate (n-1) q in quilt q' q' q' q'

renderQuilt :: Int -> FilePath -> Quilt Color -> IO ()
renderQuilt qSize fn q = do
  let q' r c = interp q (2*(fromIntegral r / fromIntegral qSize) - 1)
                 (-(2*(fromIntegral c / fromIntegral qSize) - 1))
      img    = ImageRGB8 $ generateImage (\r c -> toPixel $ q' r c) qSize qSize
  savePngImage fn img

toPixel :: Color -> PixelRGB8
toPixel (toSRGB -> RGB r g b) = PixelRGB8 (conv r) (conv g) (conv b)
  where
    conv :: Double -> Word8
    conv v = fromIntegral . clamp $ floor (v * 256)
    clamp :: Int -> Int
    clamp v
      | v > 255   = 255
      | v < 0     = 0
      | otherwise = v

nudge :: Int -> Quilt a -> Quilt a
nudge 0 q = q
nudge n q = nudge (n-1) q `rot` 5

nudgy :: Quilt Color
nudgy = nudge 100 (ifQ (x <. y) (solid red) (solid blue))

opt :: Quilt a -> Quilt a
opt (QGrey c) = QGrey (opt c)
opt (QIf cond eT eF) = QIf (opt cond) (opt eT) (opt eF)
opt (QQuilt q1 q2 q3 q4) = QQuilt (opt q1) (opt q2) (opt q3) (opt q4)
opt (QMap f q) = QMap f (opt q)
opt (QZip f q1 q2) = QZip f (opt q1) (opt q2)
opt (QRot (QRot q deg1) deg2) = opt (QRot q (deg1 + deg2))
opt x = x

quiltSize :: Quilt a -> Int
quiltSize (QIf cond eT eF) = 1 + quiltSize cond + quiltSize eT + quiltSize eF
quiltSize (QQuilt q1 q2 q3 q4) = 1 + quiltSize q1 + quiltSize q2 + quiltSize q3 + quiltSize q4
quiltSize (QMap _ q) = 1 + quiltSize q
quiltSize (QZip _ q1 q2) = 1 + quiltSize q1 + quiltSize q2
quiltSize (QRot q _) = 1 + quiltSize q
quiltSize _ = 1
