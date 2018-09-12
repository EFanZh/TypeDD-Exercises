import Data.Vect

total my_length : List a -> Nat
my_length xs = loop Z xs where
    loop len [] = len
    loop len (x :: xs) = loop (S len) xs

total my_reverse : List a -> List a
my_reverse xs = loop (the (List a) []) xs where
    loop acc [] = acc
    loop acc (x :: xs) = loop (x :: acc) xs

total my_map_list : (a -> b) -> List a -> List b
my_map_list f xs = loop (the (List b) []) xs where
    loop acc [] = reverse acc
    loop acc (x :: xs) = loop (f x :: acc) xs

total my_map_vect : (a -> b) -> Vect n a -> Vect n b
my_map_vect f xs = loop [] xs where
    total loop: Vect m b -> Vect n a -> Vect (m + n) b
    loop {m} acc [] = rewrite plusZeroRightNeutral m in reverse acc
    loop acc (x :: xs) = proof_helper (loop (f x :: acc) xs) where
        proof_helper : Vect (S j + k) b -> Vect (plus j (S k)) b
        proof_helper {j} {k} vect = rewrite sym (plusSuccRightSucc j k) in vect

-- total my_map_vect : (a -> b) -> Vect n a -> Vect n b
-- my_map_vect f [] = []
-- my_map_vect f (x :: xs) = f x :: my_map_vect f xs
