import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

TupleVect : (n : Nat) -> (ty : Type) -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())
