{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

import           Codec.Picture
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.Complex
import           Data.Word

type Color  = Colour Double   -- from the 'colour' library (cabal install colour)
type Number = Double

type Quilt a = Double -> Double -> a

quilt :: Quilt a -> Quilt a -> Quilt a -> Quilt a -> Quilt a
quilt q1 q2 q3 q4 = \x y ->
  case (x < 0, y > 0) of
    (True , True)  -> q1 (2*x + 1) (2*y - 1)
    (True , False) -> q3 (2*x + 1) (2*y + 1)
    (False, True)  -> q2 (2*x - 1) (2*y - 1)
    (False, False) -> q4 (2*x - 1) (2*y + 1)

solid :: a -> Quilt a
solid c = \_ _ -> c

x :: Quilt Number
x = \x y -> x

y :: Quilt Number
y = \x y -> y

mkGrey :: Quilt Number -> Quilt Color
mkGrey q = \x y -> let n = q x y in sRGB n n n

ifQ :: Quilt Bool -> Quilt a -> Quilt a -> Quilt a
ifQ test a b = \x y -> case test x y of
  True  -> a x y
  False -> b x y

infixl 4 <.

(<.) :: Ord a => Quilt a -> Quilt a -> Quilt Bool
q1 <. q2 = \x y -> q1 x y < q2 x y

quilterate :: Int -> Quilt a -> Quilt a
quilterate 0 q = q
quilterate n q = let q' = quilterate (n-1) q in quilt q' q' q' q'

mapColor :: (Double -> Double) -> Color -> Color
mapColor f (toSRGB -> RGB r g b) = sRGB (f r) (f g) (f b)

zipColor :: (Double -> Double -> Double) -> Color -> Color -> Color
zipColor (&) (toSRGB -> RGB r1 g1 b1) (toSRGB -> RGB r2 g2 b2)
  = sRGB (r1 & r2) (g1 & g2) (b1 & b2)

instance Num Color where
  (+) = zipColor (+)
  (-) = zipColor (-)
  (*) = zipColor (*)
  abs = mapColor abs
  signum = mapColor signum

  fromInteger i = sRGB i' i' i'
    where i' = fromInteger i

zipQuilt :: (a -> b -> c) -> Quilt a -> Quilt b -> Quilt c
zipQuilt (&) q1 q2 = \x y -> q1 x y & q2 x y

mapQuilt :: (a -> b) -> Quilt a -> Quilt b
mapQuilt f q = \x y -> f (q x y)

instance Num a => Num (Quilt a) where
  (+) = zipQuilt (+)
  (-) = zipQuilt (-)
  (*) = zipQuilt (*)
  abs = mapQuilt abs
  signum = mapQuilt signum
  fromInteger i = \x y -> fromInteger i

instance Fractional a => Fractional (Quilt a) where
  (/) = zipQuilt (/)
  recip = mapQuilt (1 /)
  fromRational r = \x y -> fromRational r

instance Floating a => Floating (Quilt a) where
  pi = \_ _ -> pi
  exp = mapQuilt exp
  log = mapQuilt log
  sqrt = mapQuilt sqrt
  (**) = zipQuilt (**)
  logBase = zipQuilt logBase
  sin = mapQuilt sin
  cos = mapQuilt cos
  tan = mapQuilt Prelude.tan
  asin = mapQuilt asin
  acos = mapQuilt acos
  atan = mapQuilt atan
  sinh = mapQuilt sinh
  cosh = mapQuilt cosh
  tanh = mapQuilt tanh
  asinh = mapQuilt asinh
  acosh = mapQuilt acosh
  atanh = mapQuilt atanh

tx, ty, scale, rot :: Quilt Number -> Quilt a -> Quilt a
tx dx q = \x y -> q (x - dx x y) y
ty dy q = \x y -> q x (y - dy x y)
scale fact q = \x y -> q (x / fact x y) (y / fact x y)
rot deg q = \x y ->
    let rad' = pi / 180 * deg x y
    in q (x * cos rad' - y * sin rad') (x * sin rad'  + y * cos rad')


z :: Quilt (Complex Double)
z = (:+)

fromComplex :: (Complex Double -> a) -> Quilt a
fromComplex f = mapQuilt f z

mysteryCount :: Quilt Int
mysteryCount = fromComplex $ \c ->
  length . take 100 . takeWhile ((< 2) . magnitude) . iterate (f c) $ 0
  where
    f c w = w*w + c

mystery :: Quilt Color
mystery = mkGrey $ mapQuilt pickColor mysteryCount
  where
    pickColor n = logBase 2 (fromIntegral n) / 7

--grate :: Quilt Color


swirl :: Quilt Color
swirl =
    let grate = -cos (x*20*pi)/2 + 0.5
        swirl' = rot (20*(sin(50*sqrt(x*x + y*y)))) grate
    in  mkGrey swirl'    * (solid yellow) + 
        mkGrey ((y+1)/2) * (solid blue)

simple = let grate = -cos (x*20*pi)/2 + 0.5 
    in mkGrey $ rot (40*(0.5-(x*x + y*y))) grate

test = renderQuilt 256 "quilt.png



renderQuilt :: Int -> FilePath -> Quilt Color -> IO ()
renderQuilt qSize fn q = do
  let q' r c = q (2*(fromIntegral r / fromIntegral qSize) - 1)
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