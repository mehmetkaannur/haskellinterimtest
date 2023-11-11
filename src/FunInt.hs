module FunInt where

import Data.List

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
applyOp "+" num1 num2  = Number (num1 + num2)
applyOp "*" num1 num2  = Number (num1 * num2)
applyOp ">" num1 num2  = Boolean (num1 > num2)
applyOp "==" num1 num2 = Boolean (num1 == num2)


apply :: Expr -> [Expr] -> Environment -> Expr
-- Pre: The application is well-formed wrt arity
-- and is correctly typed.
apply (Op opr) [Number a, Number b] _ = applyOp opr a b
apply (Fun as e) es env               = eval e new_env 
  where 
    new_env = zip as es

-- The first four rules correspond to redexes; the catch-all
-- corresponds to normal forms...
eval :: Expr -> Environment -> Expr
-- Pre: the expression is well-formed wrt arity and is
-- correctly typed.
eval (Id a) env             = lookUp a env
eval (If p q r) env
  | cond                    = eval q env
  | otherwise               = eval r env
  where 
    Boolean cond            = eval p env
eval (Let v e e') env = eval e' new_env
  where
    new_env = [(v, eval e env)]
eval (App exp1 listExp) env = apply exp1 (helper listExp) env
  where
    helper :: [Expr] -> [Expr]
    helper []     = []
    helper (x:xs) = eval x env : helper xs
eval other _                = other

--
-- Given.
--
runProgram :: Program -> Expr
runProgram p = eval p emptyEnv

freeVars :: Expr -> [Identifier]
freeVars (Number _)   = []
freeVars (Boolean _)  = []
freeVars (Op _)       = []
freeVars (Id x)       = [x]
freeVars (Let x e e') = freeVars e `union` freeVars e' \\ [x]
freeVars (Fun x e)    = freeVars e \\ x
freeVars (If p q r)   = freeVars p ++ freeVars q ++ freeVars r
freeVars (App x es)   = freeVars x ++ helper es
  where
    helper :: [Expr] -> [Identifier]
    helper []     = []
    helper (e:es) = freeVars e ++ helper es

isWellFormed :: Expr -> Bool
isWellFormed exp = freeVars exp == []

----------------------------------------------------------------------
-- Part II

maybeApply :: Expr -> [Expr] -> Environment -> Maybe Expr
-- Pre: The application is well-formed wrt arity.
maybeApply (Op opr) [Number a, Number b] _ = Just (applyOp opr a b)
maybeApply (Op opr) _ _                    = Nothing
maybeApply (Fun as e) es env               = Just (eval e new_env)
  where 
    new_env = zip as es
maybeApply _ _ _                           = Nothing

maybeEval :: Expr -> Environment -> Maybe Expr
-- Pre: the expression is well-formed wrt arity
maybeEval (Id a) env  = Just (lookUp a env)
maybeEval (If p q r) env
  | cond /= True && cond /= False = Nothing
  | cond                          = Just (eval q env)
  | otherwise                     = Just (eval r env)
  where 
    Boolean cond = eval p env
maybeEval (Let v e e') env        = Just (eval e' new_env)
  where
    new_env = [(v, eval e env)]
maybeEval (App exp1@(Op x) listExp) env 
  = Just (apply exp1 (helper listExp) env)
  where
    helper :: [Expr] -> [Expr]
    helper []     = []
    helper (x:xs) = eval x env : helper xs
maybeEval (App exp1@(Fun x y) listExp) env 
  = Just (apply exp1 (helper listExp) env)
  where
    helper :: [Expr] -> [Expr]
    helper []     = []
    helper (x:xs) = eval x env : helper xs
maybeEval (App _ _) _             = Nothing
maybeEval other _                 = Just other

{-Second maybeEval test doesn"t work because
I couldn't Let input doesn't work-}

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
eval5 = eval factOf6 []

invalid1 :: Expr
invalid1 = Fun ["x"] (App (Op "+") [Id "x", Id "y"])

invalid2 :: Expr
invalid2 = If (App (Op "==") [Number 1, Number 0]) (Id "x") (Number 4)

typeError1 :: Expr
typeError1 = App (Op ">") [Number 1, Op "+"]
