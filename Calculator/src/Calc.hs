{-# LANGUAGE GADTs #-}

module Calc
    ( description
    , helpMsg
    , calc
    ) where


description :: String
description = unlines
    [ "Welcome to Maxim's calculator."
    , "This is the best calculator you have ever experienced, period."
    , "Features this calculator supports: +, -, *, /, ^."
    , "Type an expression, :help, or :quit."
    ]

helpMsg :: String
helpMsg = unlines
    [ "You can use integers or floating point values,"
    , "negation, or standard arithmetic operators + - * / ^ ."
    ]

calc :: String -> String
calc input = "You have entered: " ++ input

    {-case parse parseExpr input of
	Left err -> err
	Right expr -> show expr-}
