import Data.Vect
import Data.Vect.Views

total mergeSort : Ord a => Vect len a -> Vect len a
mergeSort input with (splitRec input)
    mergeSort [] | SplitRecNil = []
    mergeSort [x] | SplitRecOne = [x]
    mergeSort (xs ++ ys) | (SplitRecPair lrec rrec) = let lSorted = mergeSort xs | lrec
                                                          rSorted = mergeSort ys | rrec
                                                      in merge lSorted rSorted
