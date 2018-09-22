import Data.Nat.Views

total toBinaryHelper : Nat -> List Char
toBinaryHelper k with (halfRec k)
    toBinaryHelper Z | HalfRecZ = []
    toBinaryHelper (n + n) | (HalfRecEven rec) = '0' :: (toBinaryHelper n | rec)
    toBinaryHelper (S (n + n)) | (HalfRecOdd rec) = '1' :: (toBinaryHelper n | rec)

total toBinary : Nat -> String
toBinary k = pack (reverse (toBinaryHelper k))
