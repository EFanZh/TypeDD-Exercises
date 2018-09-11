import Data.Vect

total reverseProof_nil : (acc : Vect n a) -> Vect (plus n 0) a
reverseProof_nil {n} acc = rewrite plusZeroRightNeutral n in acc

total reverseProof_xs : Vect (S n + len) a -> Vect (n + (S len)) a
reverseProof_xs {n} {len} xs = rewrite sym (plusSuccRightSucc n len) in xs

total myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs where
    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' acc [] = reverseProof_nil acc
    reverse' acc (x :: xs) = reverseProof_xs (reverse' (x :: acc) xs)
