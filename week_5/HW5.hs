{-# LANGUAGE FlexibleInstances #-}

module HW5 where

import Parser
import qualified Data.Map as M

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

data ExprT = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul a b) = (*) (eval a) (eval b)
eval (Add a b) = (+) (eval a) (eval b)

evalStr :: String -> Maybe Integer
evalStr n = eval <$> (parseExp Lit Add Mul n)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where   
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr Bool where
    lit x = if x <= 0 then False else True
    add a b = a || b
    mul a b = a && b

instance Expr MinMax where
    lit x = MinMax x
    add x y = max x y
    mul x y = min x y

instance Expr Mod7 where
    lit x = Mod7 (mod x 7)
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x+y) 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x*y) 7)

class HasVars a where
    var :: String -> a

data VarExprT = Lit' Integer
    | Var' String
    | Add' VarExprT VarExprT
    | Mul' VarExprT VarExprT
    deriving (Eq, Show)

instance HasVars VarExprT where
    var s = Var' s

instance Expr VarExprT where
    lit = Lit'
    add = Add'
    mul = Mul'

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var str = (\x -> M.lookup str x)

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = (\m -> Just x)
    add x y = (\m -> (+) <$> (x m) <*> (y m))
    mul x y = (\m -> (*) <$> (x m) <*> (y m))

withVars :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer)
    -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
