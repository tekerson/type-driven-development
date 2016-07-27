
data Elem : a -> List a -> Type where
  Here : Elem x (x :: xs)
  There : Elem x xs -> Elem x (y :: xs)

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

not_in_nil : Last [] value -> Void
not_in_nil LastOne impossible
not_in_nil (LastCons _) impossible

not_last_one : (contra : (value = x) -> Void) -> Last [x] value -> Void
not_last_one contra LastOne = contra Refl
not_last_one _ (LastCons LastOne) impossible
not_last_one _ (LastCons (LastCons _)) impossible

not_last_cons : (contra : Last (x :: xs) value -> Void) ->
                   Last (y :: (x :: xs)) value -> Void
not_last_cons contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No not_in_nil
isLast [x] value = case decEq value x of
                        (Yes Refl) => Yes LastOne
                        (No contra) => No (not_last_one contra)
isLast (y :: (x :: xs)) value = case isLast (x :: xs) value of
                                     (Yes prf) => Yes (LastCons prf)
                                     (No contra) => No (not_last_cons contra)
