import Data.Vect

-- Elem : (value : a) => (xs : Vect k a) -> Type

maryInVector : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVector = There (There Here)


removeElem : (value : a) ->
             (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} ->
             Vect n a
removeElem value (value :: ys) {prf = Here} = ys
removeElem {n = Z} value (y :: []) {prf = There later} = absurd later
removeElem {n = (S k)} value (y :: ys) {prf = There later} = y :: removeElem value ys

not_in_nil : Elem value [] -> Void
not_in_nil Here impossible
not_in_nil (There _) impossible

not_in_tail : (notThere : Elem value xs -> Void) ->
              (notHere : (value = x) -> Void) -> Elem value (x :: xs) -> Void
not_in_tail notThere notHere Here = notHere Refl
not_in_tail notThere notHere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No not_in_nil
isElem value (x :: xs) = case decEq value x of
                              (Yes Refl) => Yes Here
                              (No notHere) =>
                                case Main.isElem value xs of
                                     (Yes prf) => Yes (There prf)
                                     (No notThere) => No (not_in_tail notThere notHere)
