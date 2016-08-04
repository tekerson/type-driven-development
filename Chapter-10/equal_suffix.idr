import Data.List.Views

total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (zs ++ [x]) ys | (Snoc rec) with (snocList ys)
    equalSuffix (xs' ++ [x]) [] | (Snoc rec) | Empty = []
    equalSuffix (xs' ++ [x]) (ys' ++ [y]) | (Snoc rec_xs) | (Snoc rec_ys)
      = if x == y
        then (equalSuffix xs' ys' | rec_xs | rec_ys) ++ [x]
        else []
