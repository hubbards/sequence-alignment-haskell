-- TODO: comment

import Data.Function
import System.Environment

import Align

main :: IO ()
main = do (f1 : f2 : _) <- getArgs
          s1 <- fmap lines (readFile f1)
          s2 <- fmap lines (readFile f2)
          let delta = 2
              alpha = \ a b -> if a == b then -2 else 1
              f = ("Loaded: " ++) . show . head
              g = \ x y -> show (run $ Prob delta alpha x y)
              h = unlines . tail
          putStrLn (f s1)
          putStrLn (f s2)
          putStrLn "Alignment:"
          putStr $ (g `on` h) s1 s2
