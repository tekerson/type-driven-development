module ListLast

public export
data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

export
total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty xs y => NonEmpty (x :: xs) y
