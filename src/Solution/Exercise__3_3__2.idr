import Data.Vect

total addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs ys = zipWith (zipWith (+)) xs ys
