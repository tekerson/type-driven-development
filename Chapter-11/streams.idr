labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbls [] = []
labelWith (lbl :: lbls) (x :: xs) = (lbl, x) :: labelWith lbls xs

label : List a -> List (Integer, a)
label = labelWith (iterate (+1) 0)
