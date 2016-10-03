total
countFrom : Integer -> Stream Integer
countFrom x = iterate (+1) x
-- countFrom x = x :: countFrom (x + 1)

total
labelFrom : Integer -> List a -> List (Integer, a)
labelFrom lbl [] = []
labelFrom lbl (x :: xs) = (lbl, x) :: labelFrom (lbl + 1) xs

label : List a -> List (Integer, a)
label = labelFrom 0
