{-# LANGUAGE GADTs #-}

import Data.Char   -- for isSpace, isDigit

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

shunt :: [Token] -> [Token]
shunt xs = helper xs [] where
    helper [] tmp = tmp
    helper (TLit x : xs) tmp = (TLit x) : (helper xs tmp)
    helper (LParen : xs) tmp = helper xs (LParen : tmp)
    helper (RParen : xs) (LParen : rest) = helper xs rest
    helper (RParen : xs) (x : rest)      = x : (helper (RParen : xs) rest)
    helper (RParen : xs) []              = undefined
    helper (op     : xs) []              = helper xs [op]
    --helper (op     : xs) (LParen : rest) = helper xs (op:LParen:rest)
    helper (op     : xs) (op' : rest) | (prec op) <= (prec op') = op' : (helper (op:xs) rest)
                                      | otherwise = helper xs (op:op':rest)

prec :: Token -> Int
prec TTimes = 2
prec TPlus  = 1
prec TMinus = 1
prec LParen = 0
prec RParen = 0
prec _      = undefined
