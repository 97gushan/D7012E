module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Skip |
    Begin [Statement] |
    Read String |
    Write Expr.T 
    deriving Show

statements = iter statement
statement = assignment ! 
            condition !
            loop !
            nop !
            begin !
            Statement.read !
            Statement.write

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

condition = (accept "if" -# Expr.parse) # (require "then" -# statement) # (require "else" -# statement) >-> buildCondition
buildCondition ((e, b1), b2) = If e b1 b2

loop = ((accept "while" -# Expr.parse) #- require "do") # statement >-> buildLoop
buildLoop (e, b) = While e b

nop = accept "skip;" >-> buildNop
buildNop _ = Skip

begin = (accept "begin" -# statements) #- require "end" >-> buildBegin
buildBegin = Begin

read = (accept "read" -# word) #- require ";" >-> buildRead
buildRead = Read

write = (accept "write" -# Expr.parse) #- require ";" >-> buildWrite
buildWrite = Write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if Expr.value cond dict > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
    parse = statement
    toString = show
