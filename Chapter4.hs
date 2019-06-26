data Term = Zero
          | Succ Term
          | Pred Term
          | IsZero Term
          | T
          | F
          | If Term Term Term
          deriving (Show, Eq, Ord)
nv :: Term -> Bool
nv Zero = True
nv (Succ t) = nv t
nv _ = False

eval1 :: Term -> Term
eval1 t = case t of
  -- Rules from 3-1
  If T x y -> x
  If F x y -> y
  If t x y -> If (eval1 t) x y

  -- Rules from 3-2
  Succ t -> Succ (eval1 t)
  Pred Zero -> Zero
  Pred (Succ t) | nv t -> t
  Pred t -> Pred (eval1 t)
  IsZero Zero -> T
  IsZero (Succ t) | nv t -> F
  IsZero t -> IsZero (eval1 t)

reduced :: Term -> Bool
reduced T = True
reduced F = True
reduced t | nv t = True
          | otherwise = False

eval :: Term -> Term
eval t | reduced t = t
       | otherwise = eval (eval1 t)


big :: Term -> Term
big t = case t of
  If t x y -> case big t of
    T -> big x
    F -> big y
  Succ t -> Succ $ big t
  Pred t -> case big t of
    Zero -> Zero
    Succ t -> t
  IsZero t -> case big t of
    Zero -> T
    Succ t -> F
  v | reduced v -> v
