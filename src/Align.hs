-- | This module contains a dynamic programming algorithm for the sequence
--   alignment problem. The algorithm is adapted from the textbook "Algorithm
--   Design" by Jon Kleinberg and Eva Tardos.
--
--   An alignment of a string @x@ of length @m > 0@ and a string @y@ of length
--   @n > 0@ is a matching of their index sets where no two pairs of indices
--   cross, i.e., a relation from the index set of @x@ to the index set of @y@
--   where the following properties hold:
--
--   * Each index of either set occures in at most one pair in the relation.
--   * If @(i, j)@ and @(k, l)@ are pairs of indices in the relation with
--     @0 < i <= m@, @0 < j <= n@, and @i < k@, then @j < l@.
--
--   An index that is not matched is called a gap and the cost of a gap is given
--   by a parameter @delta > 0@. The cost of a pair of indices @(i, j)@ is the
--   mismatch cost of aligning the @i@th character of @x@ and the @j@th
--   character of @y@, which is given by a (higher-order) parameter @alpha@. The
--   total cost of an alignment is the sum of the cost of the gaps and matched
--   indices.
--
--   The minimum cost of an alignment between the first @i@ characters in @x@
--   and the first @j@ charecters in @y@ is @opt ... i j@. For @1 < i@ and
--   @1 < j@, the following recurrence is satisfied:
--
--   @
--   opt ... i j <-> min3 (alpha i j + opt ... (i - 1) (j - 1))
--                        (delta     + opt ... (i - 1) j)
--                        (delta     + opt ... i (j - 1))
--   @
--
--   where @alpha i j@ is the mismatch cost of matching the @i@th character of
--   @x@ with the @j@th character of @y@ and @delta@ is the gap penalty. Note
--   indices are 1-based and the mismatch cost of matching equal characters is
--   usually zero.
module Align (
    Cost
  , Prob (..)
  , Prob'
  , process
  , opt
  , sol
  , pretty
  , run
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Control.Monad.State

import Text.PrettyPrint

-- | Type synonym for alignment cost.
type Cost = Int

-- | Data type for instance of sequence alignment problem.
data Prob = Prob {
    gap      :: Cost
  , mismatch :: Char -> Char -> Cost
  , left     :: String
  , right    :: String
  }

-- | Data type for processed instance of sequence alignment problem.
data Prob' = Prob' {
    gap'      :: Cost
  , mismatch' :: Int -> Int -> Cost
  , left'     :: C.ByteString
  , right'    :: C.ByteString
  }

-- | Data type for instance of sequence alignment problem, minimal alignment
--   costs for subproblems, and optimal alignment.
data Data = Data {
    inst  :: Prob
  , cost  :: M.Map (Int, Int) Cost
  , align :: S.Set (Int, Int)
  }

-- | Process raw instance of sequence alignment problem.
process :: Prob -> Prob'
process (Prob delta alpha x y) = Prob' delta alpha' x' y'
  where
    x' = C.pack ('-' : x)
    y' = C.pack ('-' : y)
    alpha' i j = alpha (x' `C.index` i) (y' `C.index` j)

-- | Computation of minimal alignment costs using state monad for memoization.
opt :: Prob' -> M.Map (Int, Int) Cost
opt (Prob' delta alpha x y) = execState (optS m n) (M.fromList $ xs ++ ys)
  where
    -- initial state
    m = C.length x - 1
    n = C.length y - 1
    xs = zip (zip [0 .. m] (repeat 0)) [i * delta | i <- [0 .. m]]
    ys = zip (zip (repeat 0) [1 .. n]) [j * delta | j <- [0 .. n]]
    -- computation of minimal alignment costs
    optS :: Int -> Int -> State (M.Map (Int, Int) Cost) Cost
    optS i j = do m <- get
                  case M.lookup (i, j) m of
                    Just r  -> return r
                    Nothing -> do r <- liftM3 (\ a b c -> min3 (alpha i j + a)
                                                               (delta + b)
                                                               (delta + c))
                                              (optS (i - 1) (j - 1))
                                              (optS (i - 1) j)
                                              (optS i (j - 1))
                                  modify (M.insert (i, j) r)
                                  return r

-- minimum of three values
min3 :: (Ord a) => a -> a -> a -> a
min3 x y z
  | x >= z && y >= z = z
  | x >= y           = y
  | otherwise        = x

-- | Optimal alignment, back tracks through memoized alignment costs to
--   construct alignment.
sol :: Prob' -> M.Map (Int, Int) Cost -> [(Int, Int)]
sol (Prob' delta _ x y) m = reverse $ helper (C.length x - 1) (C.length y - 1)
  where
    -- recursive back tracking
    helper :: Int -> Int -> [(Int, Int)]
    helper i j
      | i == 0                                   = zip (repeat 0) [1 .. j]
      | j == 0                                   = zip [1 .. i] (repeat 0)
      | m M.! (i, j) == delta + m M.! (i - 1, j) = (i, 0) : helper (i - 1) j
      | m M.! (i, j) == delta + m M.! (i, j - 1) = (0, j) : helper i (j - 1)
      | otherwise                                = (i, j) : helper (i - 1) (j - 1)

-- | Pretty print optimal alignment.
pretty :: Prob' -> [(Int, Int)] -> Doc
pretty (Prob' _ _ x y) =
  uncurry (on ($$) text) . unzip
                         . map (\ (i, j) -> (x `C.index` i, y `C.index` j))

-- | Compute solution to instance of sequence alignment problem and pretty print
--   optimal alignment.
--
-- -----------------------------------------------------------------------------
-- Example for dictionary interface / spell-checking:
--
-- >>> :{
-- let
--   delta = 2
--   alpha x y
--     | x == y                                                       = 0
--     | S.member x s && S.member y s || S.member x t && S.member y t = 1
--     | otherwise                                                    = 3 where
--     s = S.fromList "aeiouy"
--     t = S.fromList ['a' .. 'z'] S.\\ s
-- in
--   run (Prob delta alpha "name" "naem")
-- :}
-- na-me
-- naem-
--
-- -----------------------------------------------------------------------------
-- Example for DNA sequence alignment:
--
-- >>> :{
-- let
--   delta     = 1
--   alpha x y = if x == y then -2 else 1
-- in
--   run (Prob delta alpha "ACACACTA" "AGCACACA")
-- :}
-- A-CACACTA
-- AGCACAC-A
--
run :: Prob -> Doc
run p = pretty p' $ sol p' (opt p')
  where
    p' = process p
