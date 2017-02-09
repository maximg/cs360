{-# LANGUAGE GADTs #-}

import Data.Char   -- for isSpace, isDigit

data Op where
  Plus  :: Op
  Minus :: Op
  Times :: Op
  deriving (Show, Eq)

data Arith where
  Lit :: Integer -> Arith
  Bin :: Op -> Arith -> Arith -> Arith
  deriving (Show)

interpArith :: Arith -> Integer
interpArith (Lit x) = x
interpArith (Bin Plus  l r) = interpArith l + interpArith r
interpArith (Bin Minus l r) = interpArith l - interpArith r
interpArith (Bin Times l r) = interpArith l * interpArith r


data Token where
  TLit   :: Integer -> Token
  TPlus  :: Token
  TMinus :: Token
  TTimes :: Token
  LParen :: Token
  RParen :: Token
  deriving (Show, Eq)


tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(' : xs) = LParen : (tokenize xs)
tokenize (')' : xs) = RParen : (tokenize xs)
tokenize ('+' : xs) = TPlus  : (tokenize xs)
tokenize ('-' : xs) = TMinus : (tokenize xs)
tokenize ('*' : xs) = TTimes : (tokenize xs)
tokenize (' ' : xs) = tokenize xs
tokenize xs         = let (lit,rest) = span isDigit xs
                      in TLit (read lit) : (tokenize rest)

-- FIXME: swap the order of arguments for helper
shunt :: [Token] -> [Token]
shunt xs = helper xs [] where
    helper [] tmp = tmp
    helper (TLit x : xs) tmp = (TLit x) : (helper xs tmp)
    helper (LParen : xs) tmp = helper xs (LParen : tmp)
    helper (RParen : xs) (LParen : rest) = helper xs rest
    helper (RParen : xs) (x : rest)      = x : (helper (RParen : xs) rest)
    helper (RParen : xs) []              = undefined
    helper (op     : xs) []              = helper xs [op]
    helper (op     : xs) (op' : rest) | (prec op) <= (prec op') = op' : (helper (op:xs) rest)
                                      | otherwise = helper xs (op:op':rest)
    prec TTimes = 2
    prec TPlus  = 1
    prec TMinus = 1
    prec LParen = 0
    prec RParen = 0
    prec _      = undefined


parsePostfix :: [Token] -> Arith
parsePostfix = head . helper [] where
    helper :: [Arith] -> [Token] -> [Arith]
    helper out [] = out
    helper out ((TLit x):xs) = helper ((Lit x):out) xs
    helper (r:l:out) (op:xs) = helper ((mkOp op):out) xs where
        mkOp TPlus  = Bin Plus l r
        mkOp TMinus = Bin Minus l r
        mkOp TTimes = Bin Times l r

eval :: String -> Integer
eval = interpArith . parsePostfix . shunt . tokenize
