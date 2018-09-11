data Vect : (len : Nat) -> (elem : Type) -> Type where
    Nil : Vect Z elem
    (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

total headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

total tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} -> (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
    decEq [] [] = Yes Refl
    decEq (x :: xs) (y :: ys) = case (decEq x y, decEq xs ys) of
                                    (Yes Refl, Yes Refl) => Yes Refl
                                    (Yes Refl, No contra) => No (tailUnequal contra)
                                    (No contra, _) => No (headUnequal contra)
