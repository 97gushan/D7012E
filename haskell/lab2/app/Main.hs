module Main where
import Data.Char

import Lib
import Expr

main :: IO ()
-- main = print(runDiff "exp(sin(2*x))" "x")
main = print(runFn "x*x+2" "x" 3)
-- main = print(findZero "y" "cos(y)*sin(y)" 2)