module Interp where
import Prelude hiding (lookup)

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Num Int
           | Fun (Value -> E Value)

type Environment = [(Name, Value)]

data E a = Success a | Error String

unitE :: a -> E a
unitE = Success 

errorE :: String -> E a
errorE = Error 

bindE :: E a -> (a -> E b) -> E b
(Success a) `bindE` k = k a
(Error s)   `bindE` k = Error s

showE :: E Value -> String
showE (Success a) = "Success: " ++ showval a
showE (Error s)   = "Error: " ++ s

showval :: Value -> String
showval (Num i) = show i
showval (Fun f) = "<function>"

lookup :: Name -> Environment -> E Value
lookup x []        = errorE ("unbound variable: " ++ x)
lookup x ((y,b):e) = if x == y then unitE b else lookup x e

interp :: Term -> Environment -> E Value
interp (Var x) e  = lookup x e
interp (Con i) e  = unitE (Num i)
interp (Add u v) e = interp u e `bindE` (\a ->
                     interp v e `bindE` (\b ->
                     add a b))
interp (Lam x v) e = unitE (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e `bindE` (\f ->
                     interp u e `bindE` (\a ->
                     apply f a))

add :: Value -> Value -> E Value
add (Num i) (Num j) = unitE (Num (i+j))
add a b             = errorE ("should be numbers: " ++ showval a
                                             ++ "," ++ showval b)

apply :: Value -> Value -> E Value
apply (Fun k) a = k a
apply f a       = errorE ("should be function: " ++ showval f) 

test :: Term -> String
test t = showE (interp t [])

term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 10) (Con 11)))

term1 = (App (Con 1) (Con 2))
