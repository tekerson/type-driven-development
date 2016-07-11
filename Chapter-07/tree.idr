data Tree a = Empty
            | Node (Tree a) a (Tree a)

Eq a => Eq (Tree a) where
    (==) Empty Empty = True
    (==) (Node l e r) (Node l' e' r')
      = l == l' && e == e' && r == r'
    (==) _ _ = False

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node l e r) =
    let left = foldr func acc l
        right = foldr func left r in
        func e right
