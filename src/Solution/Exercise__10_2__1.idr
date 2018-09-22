import Data.List.Views

total equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
    equalSuffix [] ys | Empty = []
    equalSuffix (xs ++ [x]) ys | (Snoc recXs) with (snocList ys)
        equalSuffix (xs ++ [x]) [] | (Snoc recXs) | Empty = []
        equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc recXs) | (Snoc recYs) =
            if x == y
            then equalSuffix xs ys | recXs | recYs ++ [x]
            else []
