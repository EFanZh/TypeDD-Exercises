data ThreeEq : a -> b -> c -> Type where
    MkThreeEq : ThreeEq x x x

total allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z MkThreeEq = MkThreeEq
