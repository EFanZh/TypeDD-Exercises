over_length: Nat -> List String -> Nat
over_length n strs = length (filter (\s => length s > n) strs)
