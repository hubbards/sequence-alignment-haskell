-- | This module contains an example of dynamic programming in Haskell using two
--   techniques (one based on functional arrays and the other based on the state
--   monad). A recursive version is included for comparison. Note that time and
--   memory for evaluation can checked in GHCi by setting @:set +s@ option.
--
module Fib
  (
      fibA
    , fibS
    , fibR
  ) where

import qualified Data.Array as A
import qualified Data.IntMap as M
import Control.Monad.State

-- | Computation of Fibonacci numbers using functional array for memoization.
--   The argument should be a natural number.
--
--   Examples:
--
--   >>> fibA 1
--   1
--
--   >>> fibA 2
--   1
--
--   >>> fibA 6
--   8
--
fibA :: Int -> Integer
fibA n
  | n <= 2    = 1
  | otherwise = a A.! n where
                  a =
                    A.array (1, n)
                            ([(1, 1), (2, 1)] ++
                             [(i, a A.! (i - 1) + a A.! (i - 2)) | i <- [3..n]])

-- | Computation of Fibonacci numbers using state monad for memoization. The
--   argument should be a natural number.
--
--   Examples:
--
--   >>> fibS 1
--   1
--
--   >>> fibS 2
--   1
--
--   >>> fibS 6
--   8
--
fibS :: Int -> Integer
fibS n = evalState (s n) (M.fromList [(1, 1), (2, 1)]) where
  s :: Int -> State (M.IntMap Integer) Integer
  s i = do m <- get
           case M.lookup i m of
             Just r  -> return r
             Nothing -> do r <- liftM2 (+) (s $ i - 1) (s $ i - 2)
                           modify (M.insert i r)
                           return r

-- | Computation of Fibonacci numbers using recursion. The argument should be a
--   natural number.
--
--   Examples:
--
--   >>> fibR 1
--   1
--
--   >>> fibR 2
--   1
--
--   >>> fibR 6
--   8
--
fibR :: Int -> Integer
fibR 1 = 1
fibR 2 = 1
fibR n = (fibR $ n - 1) + (fibR $ n - 2)