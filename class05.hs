{-# LANGUAGE GADTs #-}

-- http://ozark.hendrix.edu/~yorgey/360/f16/modules/05-arith-pretty.html

data Arith1 where
  Lit1 :: Integer -> Arith1
  Add  :: Arith1 -> Arith1 -> Arith1
  Sub  :: Arith1 -> Arith1 -> Arith1
  Mul  :: Arith1 -> Arith1 -> Arith1
  deriving (Show)

arithExample :: Arith1
arithExample = Add (Mul (Lit1 4) (Lit1 5)) (Lit1 2)

interpArith1 :: Arith1 -> Integer
interpArith1 (Lit1 x) = x
interpArith1 (Add l r) = interpArith1 l + interpArith1 r
interpArith1 (Sub l r) = interpArith1 l - interpArith1 r
interpArith1 (Mul l r) = interpArith1 l * interpArith1 r

braces :: String -> String
braces e = "(" ++ e ++ ")"

prettyArith1 :: Arith1 -> String
prettyArith1 (Lit1 x) = show x
prettyArith1 (Add l r) = braces $ prettyArith1 l ++ "+" ++ prettyArith1 r
prettyArith1 (Sub l r) = braces $ prettyArith1 l ++ "-" ++ prettyArith1 r
prettyArith1 (Mul l r) = braces $ prettyArith1 l ++ "*" ++ prettyArith1 r


braces' :: Int -> Int -> String -> String
braces' n1 n2 e | n1 > n2 = "(" ++ e ++ ")"
                | otherwise = e


prettyArith1' :: Arith1 -> Int -> String
prettyArith1' (Lit1 x) _ = show x
prettyArith1' (Add l r) n = braces' n 0 $ prettyArith1' l 0 ++ "+" ++ prettyArith1' r 0
prettyArith1' (Sub l r) n = braces' n 1 $ prettyArith1' l 1 ++ "-" ++ prettyArith1' r 1
prettyArith1' (Mul l r) n = braces' n 2 $ prettyArith1' l 2 ++ "*" ++ prettyArith1' r 2

prettyArith1'' x = prettyArith1' x 0


data Op where
  Plus  :: Op
  Minus :: Op
  Times :: Op
  deriving (Show, Eq)

data Arith where
  Lit :: Integer -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  deriving (Show)

data Associativity where
  L :: Associativity
  R :: Associativity
  deriving (Show, Eq)

type Precedence = Int

interpArith :: Arith -> Integer
interpArith (Lit x) = x
interpArith (Bin Plus l r)  = interpArith l + interpArith r
interpArith (Bin Minus l r) = interpArith l - interpArith r
interpArith (Bin Times l r) = interpArith l * interpArith r

assoc :: Op -> Associativity
assoc _ = L

prec :: Op -> Precedence
prec Times = 1
prec _     = 0


prettyPrec :: Precedence -> Associativity -> Arith -> String
prettyPrec _ _ (Lit x) = show x
prettyPrec p _ (Bin op l r) = 
    let braces s | p > (prec op) = "(" ++ s ++ ")"
                 | otherwise = s
        prettyPrec' = prettyPrec (prec op) (assoc op)
        sym Plus  = "+"
        sym Minus = "-"
        sym Times = "*"
    in braces $ prettyPrec' l ++ (sym op) ++ prettyPrec' r

prettyArith :: Arith -> String
prettyArith = prettyPrec 0 L

