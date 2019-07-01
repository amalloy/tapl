module Lib where
import Prelude hiding (succ, pred)

newtype Var = Var Char deriving (Eq, Ord)
data Term = Lam Var Term
          | App Term Term
          | Ref Var

type Env = [(Var, Term)]

eval :: Term -> Env -> Maybe Term
eval (Ref r) env = lookup r env
eval (App f x) env = do
  arg <- eval x env
  body <- eval f env
  case body of
    Lam v t -> eval t $ (v, arg) : env
    _ -> Nothing
eval t@(Lam v b) env = pure t

lam :: [Char] -> Term -> Term
lam [] t = t
lam (v:vs) t = Lam (Var v) (lam vs t)

ref :: Char -> Term
ref = Ref . Var

app :: Term -> [Term] -> Term
app f [] = f
app f (x:xs) = app (App f x) xs

true, false :: Term
true = lam "tf" (ref 't')
false = lam "tf" (ref 'f')

zero, succ :: Term
zero = lam "sz" (ref 'z')
succ = lam "nsz" (App (ref 's')
                   (app (ref 'n')
                     [ref 's', ref 'z']))

plus :: Term
plus = lam "nmsz" $ app (ref 'm') [ref 's', (app (ref 'n') [ref 's', ref 'z'])]

isZero :: Term
isZero = lam "n" (app (ref 'n')
                   [lam "x" false, true])

pair, first, second :: Term
pair = lam "adf" (app (ref 'f') [ref 'a', ref 'd'])
first = lam "p" (App (ref 'p') true)
second = lam "p" (App (ref 'p') false)

bind :: Char -> Term -> Term -> Term
bind v x b = App (Lam (Var v) b) x

pred :: Term
pred = bind 'c' pair $ lam "n" (App first (app (ref 'n') [next, start]))
  where start = app (ref 'c') [zero, zero]
        next = lam "p" (bind 'r' (App second (ref 'p'))
                        (app (ref 'c') [ref 'r', App succ (ref 'r')]))

times, expt :: Term
times = lam "mn" (app m [App plus (ref 'n'), zero])
-- expt = \mn.m(times n)1
expt = lam "mn" (app m [App times (ref 'n'), App succ zero])

nil, cons, isNil, hd, tl :: Term
nil = lam "cn" (ref 'n') -- \cn.n
-- cons = \xlcn.cx(lcn)
cons = lam "xlcn" (app (ref 'c') [ref 'x', app (ref 'l') [ref 'c', ref 'n']])
--isNil = \l.l(\x.false)true
--head = \l.l(\ab.a)(\a.a)
--tail = \lcn.(first (l (\xp. (pair (second p) (c x (second p)))) (pair n n)))


intChurch :: Int -> Term
intChurch n = iterate (App succ) zero !! n

format :: Term -> String
format (Lam (Var v) t) = '\\' : v : '.' : format t
format (App f x) = "(" ++ format f ++ ")(" ++ format x ++ ")"
format (Ref (Var v)) = [v]
