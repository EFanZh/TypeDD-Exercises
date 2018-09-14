data Elem : a -> List a -> Type where
    Here : Elem x (x :: xs)
    There : Elem x xs -> Elem x (y :: xs)
