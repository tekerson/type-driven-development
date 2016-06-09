import Data.Fin

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] ys = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

idx : Fin n -> Vect n a -> a
idx FZ (x :: xs) = x
idx (FS k) (x :: xs) = idx k xs

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = flip idx xs <$> integerToFin i n

take : (n : Nat) -> Vect (n + m) a -> Vect n a
take Z xs = []
take (S k) (x :: xs) = x :: take k xs

drop : (n : Nat) -> Vect (n + m) a -> Vect m a
drop Z xs = xs
drop (S k) (x :: xs) = drop k xs

sum : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sum {n} pos xs ys = case integerToFin pos n of
                         Nothing => Nothing
                         (Just i) => Just (idx i xs + idx i ys)
