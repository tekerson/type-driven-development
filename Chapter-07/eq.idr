occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case x == item of
                                  False => occurrences item xs
                                  True => 1 + occurrences item xs
