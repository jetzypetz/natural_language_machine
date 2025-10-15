{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lambek where

import LTypes

dec_rinv :: [LTp] -> LTp -> Bool
dec_rinv gamma t = undefined

dec_lfoc :: [LTp] -> LTp -> [LTp] -> Atm -> Bool
dec_lfoc gammaL t gammaR y = undefined

isGrammatical :: Lexicon a -> [String] -> Bool
isGrammatical lex ws = undefined

