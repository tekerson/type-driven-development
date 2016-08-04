import Data.List.Views

palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec)
    = if x == y
      then palindrome ys | rec
      else False
