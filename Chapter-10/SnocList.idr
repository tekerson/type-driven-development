module SnocList

-- data SnocList ty = Nil | Snoc (SnocList ty) ty

public export
data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

public export
snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs
  where
    snocListHelp : SnocList input -> (xs : List a) -> SnocList (input ++ xs)
    snocListHelp {input} snoc [] = rewrite appendNilRightNeutral input in snoc
    snocListHelp {input} snoc (x :: xs)
      = rewrite appendAssociative input [x] xs in
                (snocListHelp (Snoc snoc {x}) xs)

my_reverse_help' : (input : List a) -> SnocList input -> List a
my_reverse_help' [] Empty = []
my_reverse_help' (xs ++ [x]) (Snoc rec) = x :: my_reverse_help' xs rec

total
my_reverse' : List a -> List a
my_reverse' xs = my_reverse_help' xs (snocList xs)

total
my_reverse : List a -> List a
my_reverse input with (snocList input)
  my_reverse [] | Empty = []
  my_reverse (xs ++ [x]) | (Snoc rec) = x :: my_reverse xs | rec

-- reverse_snoc : SnocList ty -> List ty
-- reverse_snoc [] = []
-- reverse_snoc (Snoc xs x) = x :: reverse_snoc xs
