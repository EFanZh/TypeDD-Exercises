data Vect : (len : Nat) -> (elem : Type) -> Type where
    Nil : Vect Z elem
    (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

Eq elem => Eq (Vect len elem) where
    (==) [] [] = True
    (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect len) where
    foldr func init [] = init
    foldr func init (x :: xs) = func x (foldr func init xs)
