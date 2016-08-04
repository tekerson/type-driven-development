import Data.Vect
import Data.Vect.Views

total
merge_sort : Ord a => Vect n a -> Vect n a
merge_sort xs with (splitRec xs)
  merge_sort [] | SplitRecNil = []
  merge_sort [x] | SplitRecOne = [x]
  merge_sort (lefts ++ rights) | (SplitRecPair lrec rrec)
    = merge (merge_sort lefts | lrec) (merge_sort  rights | rrec)
