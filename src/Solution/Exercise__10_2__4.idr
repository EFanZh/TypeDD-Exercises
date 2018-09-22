import Data.List.Views

total palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
    palindrome [] | VNil = True
    palindrome [x] | VOne = True
    palindrome (x :: (ys ++ [y])) | (VCons rec) = x == y && palindrome ys | rec
