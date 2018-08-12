import Data.Vect

total multMatrix : Num numType => Vect n (Vect m numType) -> Vect m (Vect p numType) -> Vect n (Vect p numType)
multMatrix [] ys = []
multMatrix (x :: xs) ys = multLeftRow x ys :: multMatrix xs ys where
    multLeftRow xRow ys = sumColumn (zipWith (\x, y => map (x *) y) xRow ys) where
        sumColumn xs = foldl (zipWith (+)) (replicate _ 0) xs
