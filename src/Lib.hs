module Lib
    ( choices
    , split
    , solutions
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Op = Add | Sub | Mul | Div
  deriving Show

data Expr = Val Int | App Op Expr Expr
  deriving Show

-- Apply an operator
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Decide if the result of applying an operator to two positive
-- natural numbers is another such:
valid :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Sub x y = x > y
valid Mul x y = True
valid Div x y = x `mod` y == 0

-- Return the overall value of an expression, provided that it is
-- a positive natural number
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]

-- Return a list of all possible ways of choosing zero or more
-- elements from a list
choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs,zs <- perms ys]

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat $ map (interleave x) (perms xs)

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
               where 
                yss = subs xs

-- Return a list of all the values in an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App op l r) = values l ++ values r

-- Decide if a solution is a given solution for a given list of
-- source numbers and a target number
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns)
                  && eval e == [n]

-- Return a list of all possible ways of splitting a list into two
-- non-empty parts
split :: [a] -> [([a], [a])]
split as = (flip splitAt as) <$> [1..length as - 1]

-- Return a list of all possible expressions whose values are
-- precisely a given list of numbers
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns
              , l <- exprs ls
              , r <- exprs rs
              , e <- combine l r]

-- Combine two expressions using each operator
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

-- Return a list of all possble expressions that solve an instance
-- of the countdown problem:
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
                    , e <- exprs ns'
                    , eval e == [n]]