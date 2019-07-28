import Test.DocTest ( doctest )

import Fib

main :: IO ()
main = doctest [
    "src/Fib.hs"
  , "src/Align.hs"
  ]
