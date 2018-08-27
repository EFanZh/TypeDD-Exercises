import Data.Vect

total Matrix : Nat -> Nat -> Type
Matrix rows cols = Vect rows (Vect cols Double)
