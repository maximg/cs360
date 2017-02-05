-- CSCI 360, Fall 2016
-- Project 3: the Quilt language

module Quilt where

-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.
type Color = [Double]

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color

-- | Right now, this function ignores the input and simply produces a
--   blue image.  Obviously, you should make this function more
--   interesting!
evalQuilt :: String -> Either String QuiltFun
evalQuilt s = Right $ \x y -> [0,0,1]
