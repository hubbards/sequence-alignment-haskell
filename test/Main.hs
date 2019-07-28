import Test.DocTest ( doctest )

main :: IO ()
main = doctest ["src/Fib.hs", "src/Align.hs"]
