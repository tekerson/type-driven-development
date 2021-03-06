import Data.List.Views

total
merge_sort : Ord a => List a -> List a
merge_sort xs with (splitRec xs)
  merge_sort [] | SplitRecNil = []
  merge_sort [x] | SplitRecOne = [x]
  merge_sort (lefts ++ rights) | (SplitRecPair lrec rrec)
    = merge (merge_sort lefts | lrec) (merge_sort rights | rrec)
