{-|
Module: A2
Description: Assignment 2
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2023
-}
-- This lists what this module exports. Don't change this!
module A2
  (
    run,
    eval
  )
where

-- You *may not* add imports from Data.Map, or any other imports
import A2Types(Expr(..), Value(..), Env)
import qualified Data.Map (lookup, insert, empty)


-- | Runs a Godel expression by calling `eval` with the empty environment
run :: Expr -> Value
run e = eval Data.Map.empty e


-- | An interpreter for the Godel language.
eval :: Env -> Expr -> Value
eval env (Literal (Closure a b c)) = Error "Literal"
eval env (Literal (Error e)) = Error "Literal" -- error Literal if its closure or Error
eval env (Literal v) = v -- Every other thing is good


eval env (Plus a b)  = case ((eval env a), (eval env b)) of
    (Num x, Num y) -> Num(x + y) -- todo
    (Error x, y) -> Error "Plus"
    (x, Error y) -> Error "Plus"
    (x, y) -> Error "Plus" -- todo
 

eval env (Times a b)  = case ((eval env a), (eval env b)) of
  (Num x, Num y) -> Num(x * y)
  (Error x, y) -> Error "Times"
  (x, Error y) -> Error "Times"
  (x, y) -> Error "Times"



eval env (Equal a b)  = case ((eval env a), (eval env b)) of
    (Error x, y) -> Error "Equal"
    (x, Error y) -> Error "Equal"
    (x, y) -> if x == y then T else F

   
eval env (If a b c)  = case eval env a of
      Error a -> Error "If"
      F -> eval env c
      _ -> eval env b

eval env (Cons a b)  = case ((eval env a), (eval env b)) of
    (Error x, y) -> Error x
    (x, Error y) -> Error y
    (x, y) -> Pair x y

eval env (First a)  = case eval env a of
   (Pair x y) -> x
   _ -> Error "First"

eval env (Rest a)  = case eval env a of
   (Pair x y) -> y
   _ -> Error "Rest"

eval env (Var name)  = case (Data.Map.lookup name env) of
    Just a  -> a -- "a" is of type Value 
    Nothing -> Error "Var" -- "name" is not found in "env"

eval env (Lambda identifiers body) = case (unique identifiers == identifiers) of -- todo what other cases are missing?
    False -> Error "Lambda"
    _ -> Closure identifiers env body

eval env (App fnExpr argExprs) = case eval env fnExpr of
    (Closure identifiers env body) -> if length identifiers == length argExprs then 
      case eval_args env argExprs [] of
      [Error a] -> Error a
      values -> eval (create_env env identifiers values) body 
      else Error "App"
    _ -> Error "App"


-- Helper for evaluating each of the argExprs
eval_args :: Env -> [Expr] -> [Value] -> [Value]
eval_args env [] values = reverse values
eval_args env (expr:rest) values = case eval env expr of
    Error a -> [Error a]
    val -> eval_args env rest (val:values)


-- Helper for populating env for function body
create_env :: Env -> [String] -> [Value] -> Env
create_env env [] [] = env
create_env env (id:rest_id) (arg:rest_args) = create_env (Data.Map.insert id arg env) rest_id rest_args

-- | Helper function to obtain a list of unique elements in a list
-- Example:
--   ghci> unique [1, 2, 3, 4]
--   [1,2,3,4]
--   ghci> unique [1, 2, 3, 4, 4]
--   [1,2,3,4]
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | elem x xs = unique xs
  | otherwise = x : unique xs