module Main where
import Data.Char

import Lib
import Expr

main :: IO ()
main = print(run "exp(sin(2*x))" "x")
