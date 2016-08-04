import SnocList

total
isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1)
  isSuffix [] input2 | Empty = True
  isSuffix (xs ++ [x]) input2 | (Snoc xs_rec) with (snocList input2)
    isSuffix (xs ++ [x]) [] | (Snoc xs_rec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xs_rec) | (Snoc ys_rec)
      = if x == y then isSuffix xs ys | xs_rec | ys_rec
                  else False
