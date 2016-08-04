
data SplitList : List a -> Type where
  SplitNil : SplitList []
  SplitOne : SplitList [x]
  SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

splitList : (xs : List a) -> SplitList xs
splitList xs = splitList' xs xs
  where
    splitList' : (counter : List a) -> (input : List a) -> SplitList input
    splitList' _ [] = SplitNil
    splitList' _ [x] = SplitOne
    splitList' (_ :: _ :: counter) (item :: items)
      = case splitList' counter items of
             SplitNil => SplitOne
             SplitOne {x} => SplitPair [item] [x]
             (SplitPair lefts rights) => SplitPair (item :: lefts) rights
    splitList' _ items = SplitPair [] items

merge_sort : Ord a => List a -> List a
merge_sort xs with (splitList xs)
  merge_sort [] | SplitNil = []
  merge_sort [x] | SplitOne = [x]
  merge_sort (lefts ++ rights) | (SplitPair lefts rights)
    = merge (merge_sort lefts) (merge_sort rights)
