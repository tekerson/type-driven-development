import Data.Primitives.Views

data Face = Head | Tail

getFace : Int -> Face
getFace x with (divides x 2)
  getFace ((2 * div) + rem) | (DivBy prf)
    = case rem of
           0 => Head
           other => Tail

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z xs = []
coinFlips (S k) (x :: xs) = getFace x :: coinFlips k xs
