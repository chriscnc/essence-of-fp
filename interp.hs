module Interp where
import Prelude hiding (lookup)

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong   = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

lookup :: Name -> Environment -> Value
lookup x []        = Wrong
lookup x ((y,b):e) = if x == y then b else lookup x e

interp :: Term -> Environment -> Value
interp (Var x) e  = lookup x e
interp (Con i) e  = Num i
interp (Add u v) e = add (interp u e) (interp v e)
interp (Lam x v) e = Fun (\a -> interp v ((x,a):e))
interp (App t u) e = apply (interp t e) (interp u e)

add :: Value -> Value -> Value
add (Num i) (Num j) = Num (i+j)
add a b             = Wrong

apply :: Value -> Value -> Value
apply (Fun k) a = k a
apply f a       = Wrong

test :: Term -> String
test t = showval (interp t [])

term0 = App (Lam "x" (Add (Var "x") (Var "x")))
            (Add (Con 10) (Con 11))
