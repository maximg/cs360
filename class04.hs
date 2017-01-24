{-# LANGUAGE GADTs #-}

-- http://ozark.hendrix.edu/~yorgey/360/f16/modules/04-syntax-semantics.html

data Bin where
    Hash :: Bin
    Layer :: Bin -> Bin -> Bin
    deriving Show

prettyBin :: Bin -> String
prettyBin Hash = "#"
prettyBin (Layer x y) = "(" ++ prettyBin x ++ prettyBin y ++ ")"


parseBin :: String -> (Bin, String)
parseBin ('#' : rest) = (Hash, rest)
parseBin ('(' : rest) =
    case parseBin rest of
        (x, rest') -> case parseBin rest' of
            (y, ')' : rest'') -> (Layer x y, rest'')
