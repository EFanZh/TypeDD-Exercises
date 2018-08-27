total TupleVect : (n : Nat) -> Type -> Type
TupleVect Z t = ()
TupleVect (S k) t = (t, TupleVect k t)
