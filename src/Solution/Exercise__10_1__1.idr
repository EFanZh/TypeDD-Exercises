data TakeN : List a -> Type where
    Fewer : TakeN xs
    Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

total takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) = case takeN k xs of
                            Fewer => Fewer
                            Exact k_xs => Exact (x :: k_xs)

partial groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
    groupByN n xs | Fewer = [xs]
    groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest
