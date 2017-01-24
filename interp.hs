module Interp where
import Prelude hiding (lookup)

type Name = String

data Term = Var Name
          | Const Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

data Value = Wrong
           | Num Int
           | Fun (Value -> S Value) 

type Environment = [(Name, Value)]

type State = Int
type S a = State -> (a, State)

interp :: Term -> Environment -> S Value
interp (Var x) e  = lookup x e
interp (Const i) e  = unitS (Num i)
interp (Add u v) e = interp u e `bindS` (\a ->
                     interp v e `bindS` (\b ->
                     add a b))
interp (Lam x v) e = unitS (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e `bindS` (\f ->
                     interp u e `bindS` (\a ->
                     apply f a))
interp Count e = fetchS `bindS` (\i -> unitS (Num i))

add :: Value -> Value -> S Value
add (Num i) (Num j) = tickS `bindS` (\() -> unitS (Num (i+j)))
add a b             = unitS Wrong

apply :: Value -> Value -> S Value
apply (Fun k) a = tickS `bindS` (\() -> k a)
apply f a       = unitS Wrong

unitS :: a -> State -> (a, State)
unitS a s0 = (a, s0)

tickS :: S ()
tickS s = ((), s+1)

bindS :: S a -> (a -> S b) -> S b
m `bindS` k = \s0 -> let (a,s1) = m s0
                         (b,s2) = k a s1
                     in  (b,s2)

showS :: S Value -> String
showS m = let (a,s1) = m 0
          in "Value: " ++ showval a ++ "; " ++
             "Count: " ++ show s1

showval :: Value -> String
showval Wrong   = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

fetchS :: S State
fetchS s = (s, s)



lookup :: Name -> Environment -> S Value
lookup x []        = unitS Wrong
lookup x ((y,b):e) = if x == y then unitS b else lookup x e

test :: Term -> String
test t = showS (interp t [])

term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Const 10) (Const 11)))
