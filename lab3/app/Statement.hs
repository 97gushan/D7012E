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
    Repeat Expr.T Statement |
    Skip |
    Begin [Statement] |
    Read String |
    Write Expr.T 
    deriving Show

statements = iter statement
statement = assignment ! 
            condition !
            loop !
            Statement.repeat !
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

repeat = ((accept "repeat" -# statement) #- require "until") # Expr.parse #- require ";" >-> buildRepeat
buildRepeat (b, e) = Repeat e b

nop = accept "skip;" >-> buildNop
buildNop _ = Skip

begin = (accept "begin" -# statements) #- require "end" >-> buildBegin
buildBegin = Begin

read = (accept "read" -# word) #- require ";" >-> buildRead
buildRead = Read

write = (accept "write" -# Expr.parse) #- require ";" >-> buildWrite
buildWrite = Write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] dict input = []
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if Expr.value cond dict > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While cond s: stmts) dict input =
    if Expr.value cond dict > 0
    then exec (s: (While cond s:stmts)) dict input
    else exec stmts dict input

exec (Repeat cond s: stmts) dict input =
    if Expr.value cond dict == 0
    then exec (s:stmts) dict input
    else exec (s: (Repeat cond s: stmts)) dict input

exec (Assignment var val : stmts) dict input =
    exec stmts (Dictionary.insert (var, Expr.value val dict) dict) input 
exec (Skip : stmts) dict input = 
    exec stmts dict input
exec (Begin s: stmts) dict input = 
    exec (s ++ stmts) dict input
exec (Read s: stmts) dict (i:input) = 
    exec stmts (Dictionary.insert (s, i) dict) input
exec (Write s: stmts) dict input =
    Expr.value s dict: exec stmts dict input
exec _ _ _ = error "error"

toStringIndent :: Int -> T -> String
toStringIndent i (If cond bt be) =  calcIndent i ++ "if " ++ toString cond ++ " then\n" ++ toStringIndent (i+1) bt ++ "\nelse \n" ++ toStringIndent (i+1) be ++ "\n"
toStringIndent i (While cond b) = calcIndent i ++ "while " ++ toString cond ++ " do\n" ++ toStringIndent (i+1) b ++ "\n"
toStringIndent i (Repeat cond b) = calcIndent i ++ "repeat \n" ++  toStringIndent (i+1) b ++ calcIndent i ++ "until " ++ toString cond ++ "\n"
toStringIndent i (Assignment var val) = calcIndent i ++ var ++ " := " ++ toString val ++ ";\n"
toStringIndent i (Begin s) = calcIndent i ++ "begin \n" ++ foldr ((++) . toStringIndent (i+1)) "" s ++ calcIndent i ++ "end\n"
toStringIndent i Skip = calcIndent i ++ "skip;\n"
toStringIndent i (Read var) = calcIndent i ++ "read " ++ var ++ ";\n"
toStringIndent i (Write expr) = calcIndent i ++ "write " ++ toString expr ++ ";\n"  

calcIndent :: Int -> String
calcIndent 0 = ""
calcIndent i = "\t" ++ calcIndent (i-1)

instance Parse Statement where
    parse = statement
    toString = toStringIndent 0