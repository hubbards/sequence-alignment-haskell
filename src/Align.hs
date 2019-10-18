-- | This module contains a dynamic programming algorithm for the sequence
-- alignment problem. The algorithm is adapted from the textbook "Algorithm
-- Design" by Jon Kleinberg and Eva Tardos.
--
-- An alignment of a string @x@ of length @m > 0@ and a string @y@ of length
-- @n > 0@ is a matching of their index sets where no two pairs of indices
-- cross, i.e., a relation from the index set of @x@ to the index set of @y@
-- where the following properties hold:
--
-- * Each index of either set occures in at most one pair in the relation.
-- * If @(i, j)@ and @(k, l)@ are pairs of indices in the relation with
--   @0 < i <= m@, @0 < j <= n@, and @i < k@, then @j < l@.
--
-- An index that is not matched is called a gap and the cost of a gap is given
-- by a parameter @delta > 0@. The cost of a pair of indices @(i, j)@ is the
-- mismatch cost of aligning the @i@th character of @x@ and the @j@th
-- character of @y@, which is given by a (higher-order) parameter @alpha@. The
-- total cost of an alignment is the sum of the cost of the gaps and matched
-- indices.
--
-- The minimum cost of an alignment between the first @i@ characters in @x@
-- and the first @j@ charecters in @y@ is @opt ... i j@. For @1 < i@ and
-- @1 < j@, the following recurrence is satisfied:
--
-- @
-- opt ... i j = min3 (alpha i j + opt ... (i - 1) (j - 1))
--                    (delta     + opt ... (i - 1) j)
--                    (delta     + opt ... i (j - 1))
-- @
--
-- where @alpha i j@ is the mismatch cost of matching the @i@th character of
-- @x@ with the @j@th character of @y@ and @delta@ is the gap penalty. Note
-- indices are 1-based and the mismatch cost of matching equal characters is
-- usually zero.
module Align (
    Cost
  , Prob (..)
  , ProbP
  , Data (..)
  , process
  , opt
  , sol
  , run
  , pretty
  ) where

-- from package transformers
import Control.Monad.Trans.State (
    State
  , get
  , modify
  , execState
  )

-- from package bytestring
import qualified Data.ByteString.Char8 as C

-- from package containers
import qualified Data.Set as S
import qualified Data.Map as M

-- from package pretty
import Text.PrettyPrint (
    Doc
  , ($$)
  , text
  )

import Data.Function ( on )
import Control.Monad ( liftM3 )

-- | Type synonym for alignment cost.
type Cost = Int

-- | Data type for instance of sequence alignment problem.
--
-- TODO: use reader monad to simplify functions
--
data Prob = Prob {
  -- | Gap cost
    gap      :: Cost
  -- | Mismatch cost
  , mismatch :: Char -> Char -> Cost
  -- | Left string
  , left     :: C.ByteString
  -- | Right string
  , right    :: C.ByteString
  }

-- | Data type for processed instance of sequence alignment problem.
--
-- TODO: use reader monad to simplify functions
--
data ProbP = ProbP {
  -- | Processed gap cost
    gapP      :: Cost
  -- | Processed mismatch cost
  , mismatchP :: Int -> Int -> Cost
  -- | Processed left string
  , leftP     :: C.ByteString
  -- | Processed right string
  , rightP    :: C.ByteString
  }

-- | Data type for instance of sequence alignment problem, minimal alignment
-- costs for subproblems, and optimal alignment.
--
-- TODO: use this data type
--
data Data = Data {
  -- | Processed problem instance
    inst  :: ProbP
  -- | Minimum alignment cost for subproblems
  , cost  :: M.Map (Int, Int) Cost
  -- | Optimal alignment
  , align :: S.Set (Int, Int)
  }

-- | Process raw instance of sequence alignment problem.
process :: Prob -> ProbP
process (Prob delta alpha x y) = ProbP delta alpha' x' y'
  where
    x' = C.cons '-' x
    y' = C.cons '-' y
    alpha' i j = alpha (x' `C.index` i) (y' `C.index` j)

-- | Computation of minimal alignment costs using state monad for memoization.
opt :: ProbP -> M.Map (Int, Int) Cost
opt (ProbP delta alpha x y) =
  execState (optS m n) (M.fromList $ xs ++ ys)
  where
    -- initial state
    m = C.length x - 1
    n = C.length y - 1
    xs = zip (zip [0 .. m] (repeat 0)) [i * delta | i <- [0 .. m]]
    ys = zip (zip (repeat 0) [1 .. n]) [j * delta | j <- [0 .. n]]
    -- computation of minimal alignment costs
    optS :: Int -> Int -> State (M.Map (Int, Int) Cost) Cost
    optS i j = do memo <- get
                  case M.lookup (i, j) memo of
                    Just c  -> return c
                    Nothing -> do c <- liftM3 (\ a b c -> min3 (alpha i j + a)
                                                               (delta + b)
                                                               (delta + c))
                                              (optS (i - 1) (j - 1))
                                              (optS (i - 1) j)
                                              (optS i (j - 1))
                                  modify (M.insert (i, j) c)
                                  return c

-- minimum of three values
min3 :: (Ord a) => a -> a -> a -> a
min3 x y z
  | x >= z && y >= z = z
  | x >= y           = y
  | otherwise        = x

-- | Optimal alignment, back tracks through memoized alignment costs to
-- construct alignment.
sol :: ProbP -> M.Map (Int, Int) Cost -> [(Int, Int)]
sol (ProbP delta _ x y) memo = reverse (sol' m n)
  where
    m = C.length x - 1
    n = C.length y - 1
    -- recursive back tracking
    sol' :: Int -> Int -> [(Int, Int)]
    sol' i j
      | i == 0                                         = zip (repeat 0) [1 .. j]
      | j == 0                                         = zip [1 .. i] (repeat 0)
      | memo M.! (i, j) == delta + memo M.! (i - 1, j) = (i, 0) : sol' (i - 1) j
      | memo M.! (i, j) == delta + memo M.! (i, j - 1) = (0, j) : sol' i (j - 1)
      | otherwise                                      = (i, j) : sol' (i - 1) (j - 1)

-- | Compute solution to instance of sequence alignment problem.
--
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
--   run $ Prob delta alpha (C.pack "name") (C.pack "naem")
-- :}
-- na-me
-- naem-
--
-- Example for DNA sequence alignment:
--
-- >>> :{
-- let
--   delta     = 1
--   alpha x y = if x == y then -2 else 1
-- in
--   run $ Prob delta alpha (C.pack "ACACACTA") (C.pack "AGCACACA")
-- :}
-- A-CACACTA
-- AGCACAC-A
--
run :: Prob -> Doc
run p = pretty p' (sol p' $ opt p')
  where
    p' = process p

-- | Pretty print optimal alignment.
pretty :: ProbP -> [(Int, Int)] -> Doc
pretty (ProbP _ _ x y) =
  uncurry (on ($$) text) . unzip
                         . map (\ (i, j) -> (x `C.index` i, y `C.index` j))
