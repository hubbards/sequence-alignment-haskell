-- TODO: comment

-- from bytestring package
import qualified Data.ByteString.Char8 as C

import System.Environment ( getArgs )

import Align (
    Cost
  , Prob (..)
  , run
  )

main :: IO ()
main = do (file1 : file2 : _) <- getArgs
          (title1 : seq1) <- C.lines <$> C.readFile file1
          (title2 : seq2) <- C.lines <$> C.readFile file2
          let seq1' = C.unlines seq1
              seq2' = C.unlines seq2
          putStrLn ("Loaded: " ++ show title1)
          putStrLn ("Loaded: " ++ show title2)
          putStrLn "Alignment:"
          print (run $ Prob delta alpha seq1' seq2')

delta :: Cost
delta = 2

alpha :: Char -> Char -> Cost
alpha a b = if a == b then -2 else 1
