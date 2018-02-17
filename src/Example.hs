-- | This module contains examples of sequence alignments from different
--   domains.
module Example where

import qualified Data.Set as S

import Text.PrettyPrint

import Align

-- -----------------------------------------------------------------------------
-- Examples for dictionary interface / spell-checking

delta1 :: Cost
delta1 = 2

alpha1 :: Char -> Char -> Cost
alpha1 x y
  | x == y                                                       = 0
  | S.member x s && S.member y s || S.member x t && S.member y t = 1
  | otherwise                                                    = 3 where
  s = S.fromList "aeiouy"
  t = S.fromList ['a'..'z'] S.\\ s

prob1 :: String -> String -> Prob
prob1 = Prob delta1 alpha1

example1 :: Doc
example1 = run (prob1 "name" "naem")

-- -----------------------------------------------------------------------------
-- Examples for DNA sequence alignment

delta2 :: Cost
delta2 = 1

alpha2 :: Char -> Char -> Cost
alpha2 x y = if x == y then -2 else 1

prob2 :: String -> String -> Prob
prob2 = Prob delta2 alpha2

example2 :: Doc
example2 = run (prob2 "ACACACTA" "AGCACACA")
