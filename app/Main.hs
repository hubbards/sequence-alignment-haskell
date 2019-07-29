-- TODO: comment

import System.Environment

import Align

main :: IO ()
main = do (file1 : file2 : _) <- getArgs
          (title1 : seq1) <- lines <$> readFile file1
          (title2 : seq2) <- lines <$> readFile file2
          let seq1' = unlines seq1
              seq2' = unlines seq2
          putStrLn ("Loaded: " ++ title1)
          putStrLn ("Loaded: " ++ title2)
          putStrLn "Alignment:"
          print (run $ Prob delta alpha seq1' seq2')

delta :: Cost
delta = 2

alpha :: Char -> Char -> Cost
alpha a b = if a == b then -2 else 1
