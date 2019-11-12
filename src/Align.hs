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
-- and the first @j@ charecters in @y@ is @opt ... i j@. For @0 < i@ and
-- @0 < j@, the following recurrence is satisfied:
--
-- @
-- opt ... i j = min3 (alpha (i - 1) (j - 1) + opt (i - 1) (j - 1))
--                    (delta                 + opt (i - 1) j)
--                    (delta                 + opt i (j - 1))
-- @
--
-- where @alpha i j@ is the mismatch cost of matching the character at index @i@
-- of @x@ with the character at index @j@ of @y@ and @delta@ is the gap penalty.
-- Note indices are 0-based and the mismatch cost of matching equal characters
-- is usually zero.
module Align (
    Cost
  , Mismatch

  , Align
  , getLeft
  , getRight
  , getBoth
  , pretty

  , Result
  , compOpt
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
  , text
  , int
  , nest
  , (<+>)
  , ($+$)
  )

import Data.Maybe ( fromMaybe )
import Control.Arrow (
    (***)
  , (&&&)
  )
import Control.Monad ( liftM3 )

-- | Type synonym for alignment cost.
type Cost = Int

-- | Type synonym for mismatch cost function.
type Mismatch a = a -> a -> Cost

-- | Make an index-based mismatch cost function.
--
-- NOTE: ByteString index takes O(1) time
--
mkMismatch :: Mismatch Char -> C.ByteString -> C.ByteString -> Mismatch Int
mkMismatch mismatch xs ys i j = mismatch (xs `C.index` i) (ys `C.index` j)

-- | Data type for configuration.
data Config = Config {
    getGap      :: Cost
  , getMismatch :: Mismatch Int
  , getLengthL  :: Int
  , getLengthR  :: Int
  }

-- | Make a configuration.
mkConfig :: Cost -> Mismatch Char -> C.ByteString -> C.ByteString -> Config
mkConfig gap mismatch xs ys = Config {
    getGap      = gap
  , getMismatch = mkMismatch mismatch xs ys
  , getLengthL  = C.length xs -- NOTE: ByteString length takes O(1) time
  , getLengthR  = C.length ys -- NOTE: ByteString length takes O(1) time
  }

-- | Computation of minimal alignment costs using state monad for memoization.
--
-- TODO: time complexity
--
compOptCosts :: Config -> M.Map (Int, Int) Cost
compOptCosts (Config gap mismatch m n) =
  execState (comp m n) (M.fromList $ ls ++ rs) where
    -- initial state
    ls = zip (zip [0 .. m] (repeat 0)) [i * gap | i <- [0 .. m]]
    rs = zip (zip (repeat 0) [1 .. n]) [j * gap | j <- [0 .. n]]
    -- helper for computation of minimal alignment costs
    comp :: Int -> Int -> State (M.Map (Int, Int) Cost) Cost
    comp i j = do
      memo <- get
      case M.lookup (i, j) memo of
        Just cost -> return cost
        Nothing   -> do
          cost <- liftM3 (\ a b c -> min3 (mismatch (i - 1) (j - 1) + a)
                                          (gap + b)
                                          (gap + c))
                         (comp (i - 1) (j - 1))
                         (comp (i - 1) j)
                         (comp i (j - 1))
          modify (M.insert (i, j) cost)
          return cost

-- | Minimum of three values.
min3 :: (Ord a) => a -> a -> a -> a
min3 x y z
  | x >= z && y >= z = z
  | x >= y           = y
  | otherwise        = x

-- | Type synonym for alignment.
newtype Align a = Align { getAlign :: [(a, a)] }

-- | Get alignment for left string.
getLeft :: Align a -> [a]
getLeft = map fst . getAlign

-- | Get alignment for right string.
getRight :: Align a -> [a]
getRight = map snd . getAlign

-- | Get alignment for both left and right strings.
getBoth :: Align a -> ([a], [a])
getBoth = unzip . getAlign

-- TODO: replace with arrow instance
helper1 :: (a -> b) -> (a -> b) -> Align a -> Align b
helper1 f g = Align . map (f *** g) . getAlign

-- TODO: replace with arrow instance
helper2 :: (a -> b) -> Align a -> Align b
helper2 f = Align . map (both f) . getAlign

-- | Apply function to both components of pair.
--
-- TODO: import from external package
--
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- | Make alignment by backtracking through alignment costs for subproblems.
--
-- NOTE: takes O(n) time worst case
--
mkAlign :: Config -> M.Map (Int, Int) Cost -> Align Int
mkAlign (Config gap _ m n) memo = Align $ reverse (back m n) where
  -- helper for recursive back tracking
  back :: Int -> Int -> [(Int, Int)]
  back i j
    | j == 0                                       = zip [0 .. i - 1] (repeat $ -1)
    | i == 0                                       = zip (repeat $ -1) [1 .. j - 1]
    | memo M.! (i, j) == gap + memo M.! (i - 1, j) = (i - 1, -1) : back (i - 1) j
    | memo M.! (i, j) == gap + memo M.! (i, j - 1) = (-1, j - 1) : back i (j - 1)
    | otherwise                                    = (i - 1, j - 1) : back (i - 1) (j - 1)

-- | Pretty printer.
pretty :: Char -> Align (Maybe Char) -> Doc
pretty gapChar align = text xs $+$ text ys where
  align'   = helper2 (fromMaybe gapChar) align
  (xs, ys) = getBoth align'

-- | Type synonym for result consisting of cost and alignment.
type Result = (Cost, Align (Maybe Char))

-- | Computation of minimum cost and alignment.
--
-- Example for dictionary interface / spell-checking:
--
-- >>> :{
-- let
--   gap = 2
--   mismatch x y
--     | x == y                                                       = 0
--     | S.member x s && S.member y s || S.member x t && S.member y t = 1
--     | otherwise                                                    = 3 where
--     s = S.fromList "aeiouy"
--     t = S.fromList ['a' .. 'z'] S.\\ s
--   helper = pretty '-' . snd
-- in
--   helper $ compOpt gap mismatch (C.pack "name") (C.pack "naem")
-- :}
-- na-me
-- naem-
--
-- Example for DNA sequence alignment:
--
-- >>> :{
-- let
--   gap = 1
--   mismatch x y = if x == y then -2 else 1
--   helper = pretty '-' . snd
-- in
--   helper $ compOpt gap mismatch (C.pack "ACACACTA") (C.pack "AGCACACA")
-- :}
-- A-CACACTA
-- AGCACAC-A
--
compOpt :: Cost -> Mismatch Char -> C.ByteString -> C.ByteString -> Result
compOpt gap mismatch xs ys = (cost, align') where
  config@(Config _ _ m n) = mkConfig gap mismatch xs ys
  memo                    = compOptCosts config
  cost                    = memo M.! (m, n)
  align                   = mkAlign config memo
  align'                  = helper1 (sindex xs) (sindex ys) align

-- | Safe index function.
--
-- TODO: import from external package
--
sindex :: C.ByteString -> Int -> Maybe Char
sindex cs i =
  if 0 <= i && i < C.length cs
    then Just (cs `C.index` i)
    else Nothing
