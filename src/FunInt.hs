module FunInt where

-- these types must not be edited, or they will break the tests
type Program = Expr
type Identifier = String
data Expr = Number Int
          | Boolean Bool
          | Id Identifier
          | Op Identifier
          | Let Identifier Expr Expr
          | If Expr Expr Expr
          | Fun [Identifier] Expr
          | App Expr [Expr]
          deriving Show

-- this type /can/ be modified without affecting the tests
type Environment = [(Identifier, Expr)]

----------------------------------------------------------------------
-- Given...

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp k t = case lookup k t of
  Nothing -> error ("error: failed to find " ++ show k ++ " in " ++ show t)
  Just binding -> binding

----------------------------------------------------------------------
-- Part I

applyOp :: Identifier -> Int -> Int -> Expr
applyOp = undefined

apply :: Expr -> [Expr] -> Environment -> Expr
-- Pre: The application is well-formed wrt arity
-- and is correctly typed.
apply = undefined

-- The first four rules correspond to redexes; the catch-all
-- corresponds to normal forms...
eval :: Expr -> Environment -> Expr
-- Pre: the expression is well-formed wrt arity and is
-- correctly typed.
eval = undefined

--
-- Given.
--
runProgram :: Program -> Expr
runProgram p = eval p emptyEnv

isWellFormed :: Expr -> Bool
isWellFormed = undefined

----------------------------------------------------------------------
-- Part II

maybeApply :: Expr -> [Expr] -> Environment -> Maybe Expr
-- Pre: The application is well-formed wrt arity.
maybeApply = undefined

maybeEval :: Expr -> Environment -> Maybe Expr
-- Pre: the expression is well-formed wrt arity
maybeEval = undefined

message1, message2 :: String
message1 = "Type error"
message2 = "Program not well-formed (arity check)"

maybeRunProgram :: Expr -> IO()
maybeRunProgram = undefined

----------------------------------------------------------------------
-- Tests referred to in the spec.

emptyEnv :: Environment
emptyEnv = []

env :: Environment
env = [("x", Number 1), ("b", Boolean True), ("y", Number 5)]

minOf2 :: Expr
minOf2 = Fun ["a", "b"] (If (App (Op ">") [Id "a", Id "b"]) (Id "b") (Id "a"))

factOf6 :: Expr
factOf6 =
  Let "fact"
    (Fun ["x"] (If (App (Op "==") [Id "x", Number 0])
                   (Number 1)
                   (App (Op "*") [Id "x",
                                  App (Id "fact") [App (Op "+") [Id "x",
                                                                 Number (-1)]]])
               )
    )
    (App (Id "fact") [Number 6])

app1 :: Expr
app1 = apply (Op "+") [Number 7, Number 4] emptyEnv

app2 :: Expr
app2 = apply (Op ">") [Number 8, Number 2] emptyEnv

app3 :: Expr
app3 =  apply minOf2 [Number 6, Number 0] emptyEnv

eval1 :: Expr
eval1 = eval (Number 8) env

eval2 :: Expr
eval2 = eval (Id "x") env

eval3 :: Expr
eval3 = eval (Op "+") env

eval4 :: Expr
eval4 = eval (App (Op "+") [Id "x", Id "y"]) env

eval5 :: Expr
eval5 = eval factOf6 emptyEnv

invalid1 :: Expr
invalid1 = Fun ["x"] (App (Op "+") [Id "x", Id "y"])

invalid2 :: Expr
invalid2 = If (App (Op "==") [Number 1, Number 0]) (Id "x") (Number 4)

typeError1 :: Expr
typeError1 = App (Op ">") [Number 1, Op "+"]

typeError2 :: Expr
typeError2 = App (Fun ["x","y"] (App (Op "*") [Id "x", Boolean True])) [Number 5, Number 6]
