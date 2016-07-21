data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

Show num => Show (Expr num) where
  show (Val n) = show n
  show (Abs x) = "|" ++ show x ++ "|"
  show (Div x y) = show x ++ " / " ++ show y
  show (Mul x y) = show x ++ " * " ++ show y
  show (Sub x y) = show x ++ " - " ++ show y
  show (Add x y) = show x ++ " + " ++ show y

(Eq ty, Integral ty, Neg ty) => Eq (Expr ty) where
  (==) x y = eval x == eval y

(Integral num, Neg num) =>
Cast (Expr num) num where
  cast expr = eval expr

Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs (map func x)

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

Eq a => Eq (Vect n a) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = (x == y) && (xs == ys)

Foldable (Vect n) where
  foldr func acc [] = acc
  foldr func acc (x :: xs) = func x (foldr func acc xs)
