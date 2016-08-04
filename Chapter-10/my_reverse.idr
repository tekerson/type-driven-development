import ListLast

my_reverse : List a -> List a
my_reverse xs with (listLast xs)
  my_reverse [] | Empty = []
  my_reverse (xs' ++ [x]) | (NonEmpty xs' x) = x :: my_reverse xs'
