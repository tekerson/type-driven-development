data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a
%name Vect xs, ys, zs

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : (eq : EqNat j k) -> EqNat (S j) (S k)
sameS (Same j) = Same (S j)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S j) (S k) = do
  eq <- checkEqNat j k
  pure (cong eq)

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
  Nothing => Nothing
  Just Refl => Just input

same_cons : (xs : List a) -> (ys : List a) ->
            (xs = ys) -> (x :: xs = x :: ys)
same_cons xs xs Refl = Refl

same_lists : {xs : List a} -> {ys : List a} ->
             (x = y) -> (xs = ys) -> x :: xs = y :: ys
same_lists Refl Refl = Refl

data ThreeEq : (a : ty) -> (b : ty) -> (c : ty) -> Type where
  Same3 : (a : ty) -> ThreeEq a a a

allSameS : ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS (Same3 j) = Same3 (S j)

exactLength' : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength' {m} len input = case decEq len m of
                                  (Yes Refl) => Just input
                                  (No contra) => Nothing

head_unequal : DecEq a => (xs : Vect n a) -> (ys : Vect n a) ->
  (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
head_unequal xs xs contra Refl = contra Refl

tail_unequal : DecEq a => (xs : Vect n a) -> (ys : Vect n a) ->
  (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tail_unequal xs xs contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) =
    case decEq x y of
         (No contra) => No (head_unequal xs ys contra)
         (Yes Refl) =>
           case decEq xs ys of
                (No contra) => No (tail_unequal xs ys contra)
                (Yes Refl) => Yes Refl
