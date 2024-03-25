{-|
 -
Module:      A3
Description: Assignment 3
Copyright: (c) University of Toronto
               CSC324 Principles of Programming Languages, Fall 2023
-}
-- This lists what this module exports. Don't change this!

module A3 (
    -- Warmup Task
    cpsFactorial, cpsFibonacci, cpsLength, cpsMap,
    cpsMergeSort, cpsSplit, cpsMerge,
    -- Main Task
    cpsEval
) where

-- You *may not* add imports from Data.Map, or any other imports
import qualified Data.Map (Map, lookup, insert, empty, fromList)
import A3Types (Env, emptyEnv, Value(..), Expr(..))


------------------------------------------------------------------------------
-- * Warmup Task. CPS Transforming Haskell Functions *
------------------------------------------------------------------------------

-- | Compute the factorial of a number
-- factorial :: Int -> Int

-- | Compute the factorial of a number, in continuation passing style
cpsFactorial:: Int -> (Int -> r) -> r
cpsFactorial 0 k = k 1
cpsFactorial n k = cpsFactorial (n - 1) (\res -> k (n * res))

-- | Compute the n-th fibonacci number F(n).
--    Recall F(0) = 0, F(1) = 1, and F(n) = F(n-1) + F(n-2)

-- fibonacci :: Int -> Int
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2))

-- | Compute the n-th fibonacci number F(n), in continuation passing style
cpsFibonacci:: Int -> (Int -> r) -> r
cpsFibonacci 0 k = k 0
cpsFibonacci 1 k = k 1
cpsFibonacci n k = cpsFibonacci (n - 1) (\res -> cpsFibonacci (n - 2) (\res2 -> k (res + res2)))

------------------------------------------------------------------------------
-- | List functions

-- | CPS transform of the function `length`, which computes the length of a list
cpsLength :: [a] -> (Int -> r) -> r
cpsLength [] k = k 0
cpsLength (x:xs) k = cpsLength xs (\res -> k (1 + res))


-- | CPS transform of the function `map`. The argument function (to be applied
--   every element of the list) is written in direct style
cpsMap :: (a -> b) -> [a] -> ([b] -> r) -> r
cpsMap f [] k = k []
cpsMap f (x:xs) k = cpsMap f xs (\res -> k ((f x):res))

------------------------------------------------------------------------------
-- Merge Sort

-- | Sort a list using mergeSort
-- mergeSort :: [Int] -> [Int]

-- | Split a list into two lists. All list elements in even indices
-- is placed in one sub-list, and all list elements in odd indicies
-- is placed in the second sub-list.
-- split :: [Int] -> ([Int], [Int])

-- | Merge two sorted lists together
-- merge :: [Int] -> [Int] -> [Int]

-- | CPS transform of mergeSort
cpsMergeSort :: [Int] -> ([Int] -> r) -> r
cpsMergeSort [] k = k []
cpsMergeSort [x] k = k [x]
cpsMergeSort lst k = 
    cpsSplit (lst) (\c2 -> cpsMergeSort (fst c2)
                    (\c3 -> cpsMergeSort (snd c2) (
                     (\final -> cpsMerge c3 final k)
                    )))

-- | CPS transform of split
cpsSplit :: [Int] -> (([Int], [Int]) -> r) -> r
cpsSplit [] k = k ([], [])
cpsSplit [x] k = k ([x], [])
cpsSplit [x,y] k = k ([x], [y])
cpsSplit (x:y:xs) k = cpsSplit xs (\res -> k ([x] ++ fst res,[y] ++ snd res))


-- | CPS transform of merge
cpsMerge :: [Int] -> [Int] -> ([Int] -> r) -> r
cpsMerge [] [] k = k []
cpsMerge x [] k = k x
cpsMerge [] x k = k x
cpsMerge [x] [y] k = if (x >= y) then k [y,x] else k [x, y]
cpsMerge (x:xs) (y:ys) k = 
    if (x >= y) then cpsMerge (x:xs) ys (\res -> k (y:res))
    else cpsMerge xs (y:ys) (\res -> k (x:res))

------------------------------------------------------------------------------
-- * Main Task. CPS Transforming The Godel Interpreter *
------------------------------------------------------------------------------

-- | A CPS interpreter `eval` for Godel , which takes an environment,
--   an expression, and a continuation, and calls the continuation with
--   the evaluated value.
--   Notice that the type signature of `eval` is less general compared to
--   what was used above, i.e., it is not:
--      Env -> Expr -> (Value -> r) -> r
--   This restriction on the type of the continuation makes it easier
--   to define `Expr` Haskell data type, and to check for errors.
idl = \x -> x

cpsEval :: Env -> Expr -> (Value -> Value) -> Value
cpsEval env (Literal v) k = if validLiteral v then k v else Error "Literal"
cpsEval env (Lambda params body) k_lambda = 
    if params == unique params then 
        k_lambda $ Closure $ \argvals k_app ->
        -- TODO: handle errors!
        -- note that we differentiate between k_lambda: the continuation 
        -- to call after *creating the closure*, and k_app: the continuation
        -- to call after *evaluating the body of the closure* during an
        -- application.
        if length params == length argvals then
            let paramArgTuples = zip params argvals
                newEnv = foldl (\e (param, arg) -> Data.Map.insert param arg e)
                            env
                            paramArgTuples
            in cpsEval newEnv body k_app
        else Error "App"
    else Error "Lambda"
cpsEval env (Plus a b) k = cpsEval env a (\res -> cpsEval env b (\res2 -> 
    case (res, res2) of 
    (Num x, Num y) -> k $ Num (x + y)
    _              -> Error "Plus"))
cpsEval env (Times a b) k = cpsEval env a (\res -> cpsEval env b (\res2 -> 
    case (res, res2) of 
    (Num x, Num y) -> k $ Num (x * y)
    _              -> Error "Times"))
cpsEval env (Equal a b) k = cpsEval env a (\res -> cpsEval env b (\res2 -> if res == res2 then k T else k F ))
cpsEval env (Cons a b) k = cpsEval env a (\res -> cpsEval env b (\res2 -> k $ Pair res res2))
cpsEval env (First expr) k = cpsEval env expr (\res -> case (res) of
    (Pair c d) -> k c 
    _          -> Error "First")
cpsEval env (Rest expr) k = cpsEval env expr (\res -> case (res) of
    (Pair c d) -> k d 
    _          -> Error "Rest")
cpsEval env (If cond expr alt) k = cpsEval env cond (\res -> case (res) of 
    (Error c)  -> Error c
    F          -> (cpsEval env alt k)
    _          -> (cpsEval env expr k))
cpsEval env (Var name) k  = case (Data.Map.lookup name env) of
     Just a  -> k a
     Nothing -> Error "Var"
    
-- Example: shift expression
-- (Plus (Literal $ Num 2) (Shift "d" (Plus (App (Var "d") [Literal $ Num 5]) (App (Var "d") [Literal $ Num 10]))))
-- + 2 (shift d (+ (d 5) (d 10)))
-- -- d = \x -> + 2 x 

cpsEval env (Shift name body) k = 
    let newEnv = Data.Map.insert name (Closure (\[x] newK -> newK $ k x) ) env
    in cpsEval newEnv body idl
-- cpsEval env (expr (Shift name body)) k  = case (expr) of
--     (reset) -> 
--         let newEnv = Data.Map.insert name idl env     
--         in cpsEval newEnv body k
--     (Plus a) -> 
--         let newL = (\x -> cpsEval env (Plus x a)) 
--             newEnv = Data.Map.insert name newL env     
--         in cpsEval newEnv body k
--     (Times a) -> 
--         let newL = (\x -> cpsEval env (Times x a)) 
--             newEnv = Data.Map.insert name newL env     
--         in cpsEval newEnv body k

-- (* 10 (+ 2 12))



-- cpsEval {} (+ 1 ( Shift c (c 1))) id
-- cpsEval {} 1 (\r -> cpsEval {} (Shift c (c 1)) (\r2 -> id (r + r2)))
-- cpsEval {} (Shift c (c 1)) (\r2 -> 1 + r2)
-- cpsEval { c -> Closure (\[x] nk -> nk ((\r2 -> 1 + r2) x))} (c 1) id
-- id (1 + 1) = 2 

-- cpsEval {} (+ 1 (reset (+ 1 ( Shift c (c 1))))) id
-- cpsEval {} 1 (\r -> cpsEval {} (reset (+ 1 ( Shift c (c 1)))) (\r2 -> r + r2))
-- cpsEval {} (reset (+ 1 ( Shift c (c 1)))) (\r2 -> 1 + r2)
-- cpsEval {} (+ 1 ( Shift c (c 1))) (\res -> (\r2 -> 1 + r2) res) 
-- 2




-- cpsEval {} reset (shift k (* (k 3) (k 4))) (\x -> 10(x + 2 ))
-- cpsEval {} (shift k (* (k 3) (k 4))) (
--     newEnv = Data.Map.insert k (Closure (\x newK -> newK $ x)) {} -- d = {k: Closure (\x newK -> newK $ x) }
--     cpsEval d (* (k 3) (k 4)) (\l -> l)
--     cpsEval d (k 3) (\res -> cpsEval d (k 4) (\res2 -> case (res, res2) of (Num x, Num y) -> k $ Num (x * y)))

--     \res -> cpsEval d (k 4) (\res2 -> case (res, res2) of (Num x, Num y) -> k $ Num (x * y) $ 3
--     cpsEval d (k 4) (\res2 -> case (3, res2) of (Num x, Num y) -> k $ Num (x * y))
--     (\res2 -> case (3, res2) of (Num x, Num y) -> k $ Num (x * y) $ 4
--     \4 -> case (3, 4) of (Num 3, Num 4) -> id $ Num (3 * 4) = 12
-- )

-- cpsEval env a (\res -> cpsEval env b (\res2 -> 
--     case (res, res2) of 
--     (Num x, Num y) -> k $ Num (x * y)
--     _              -> Error "Times"))

cpsEval env (Reset expr) k = cpsEval env expr (\res -> k res)
    -- (Shift name body) -> 
    --     let newEnv = Data.Map.insert name (Closure (\[x] newK -> newK idl x)) env
   -- (Shift name body) -> k $ cpsEval env (Shift name body) (\res -> )





cpsEval env (App func args) k = case (cpsEval env func id) of
     (Closure f) -> 
        evalHelper args env (\vargs -> f vargs k)



-- cps style
evalHelper :: [Expr] -> Env -> ([Value] -> Value) -> Value
evalHelper [] e k = k []
evalHelper (x:xs) e k = 
    cpsEval e x (\res -> evalHelper xs e (\res2 -> k (res:res2)))
-- evalHelper (x:xs) e k =
--     [cpsEval e x (\res -> evalHelper xs e (\res2 -> k ([res] ++ res2)))]
-- evalHelper (x:xs) = 

-- Helper function (written in direct style) to identify duplicate parameters in a lambda
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
  | elem x xs = unique xs
  | otherwise = x : unique xs

-- Helper function (written in direct style) to check if a Value contains a Closure/Error
validLiteral :: Value -> Bool
validLiteral T           = True
validLiteral F           = True
validLiteral (Num n)     = True
validLiteral (Pair v w)  = (validLiteral v) && (validLiteral w)
validLiteral (Closure p) = False
validLiteral (Error e)   = False

