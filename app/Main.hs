-- TODO: comment

-- from bytestring package
import qualified Data.ByteString.Char8 as C

-- from package pretty
import Text.PrettyPrint ( Doc )

import System.Environment ( getArgs )

import Align (
    Cost
  , Mismatch
  , pretty
  , compOpt
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
          print (helper seq1' seq2')

helper :: C.ByteString -> C.ByteString -> Doc
helper xs ys = pretty gapChar align where
  (cost, align) = compOpt gap mismatch xs ys
  -- gap character
  gapChar :: Char
  gapChar = '-'
  -- gap penalty
  gap :: Cost
  gap = 2
  -- mismatch cost function
  mismatch :: Mismatch Char
  mismatch a b = if a == b then -2 else 1
