{-# LANGUAGE GADTs #-}

import Data.Char

-- http://ozark.hendrix.edu/~yorgey/360/f16/modules/04-syntax-semantics.html

data Bin where
    Hash :: Bin
    Layer :: Bin -> Bin -> Bin
    deriving Show

prettyBin :: Bin -> String
prettyBin Hash = "#"
prettyBin (Layer x y) = "(" ++ prettyBin x ++ prettyBin y ++ ")"


parseBin :: String -> Maybe (Bin, String)
parseBin ('#' : rest) = Just (Hash, rest)
parseBin ('(' : rest) =
    case parseBin rest of
        Just (x, rest') -> case parseBin rest' of
            Just (y, ')' : rest'') -> Just (Layer x y, rest'')
            _ -> Nothing
        _ -> Nothing
parseBin _ = Nothing

interpBin :: Bin -> Integer
interpBin Hash = 1
interpBin (Layer l r) = interpBin l + 2 * (interpBin r)

evalBin :: String -> Maybe Integer
evalBin xs = do
    (bin, _) <- parseBin xs
    return $ interpBin bin

{-
<digit>   ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
<ebin> ::= '#'
        | <digit>
        | '(' <ebin> <ebin> ')'
-}

data EBin where
    EHash :: EBin
    Digit :: Int -> EBin
    ELayer :: EBin -> EBin -> EBin
    deriving Show

prettyEBin :: EBin -> String
prettyEBin EHash = "#"
prettyEBin (Digit d) = show d
prettyEBin (ELayer x y) = "(" ++ prettyEBin x ++ prettyEBin y ++ ")"

parseEBin :: String -> Maybe (EBin, String)
parseEBin ('#' : rest) = Just (EHash, rest)
parseEBin ('(' : rest) =
    case parseEBin rest of
        Just (x, rest') -> case parseEBin rest' of
            Just (y, ')' : rest'') -> Just (ELayer x y, rest'')
            _ -> Nothing
        _ -> Nothing
parseEBin (c : rest) | isDigit c = Just (Digit $ read [c], rest)
                     | otherwise = Nothing
parseEBin _ = Nothing

desugar :: EBin -> Bin
desugar EHash = Hash
desugar (Digit d) = grow d where
    grow 0 = Hash
    grow n = let inner = grow (n-1) in Layer inner inner
desugar (ELayer l r) = Layer (desugar l) (desugar r)


evalEBin :: String -> Maybe Integer
evalEBin xs = do
    (ebin, _) <- parseEBin xs
    return $ interpBin $ desugar ebin

