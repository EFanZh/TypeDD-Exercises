data TakeN : List a -> Type where
    Fewer : TakeN xs
    Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

total takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) = case takeN k xs of
                            Fewer => Fewer
                            Exact k_xs => Exact (x :: k_xs)

partial halves : List a -> (List a, List a)
halves xs with (takeN (div (length xs) 2) xs)
    halves (left ++ right) | Exact left = (left, right)
