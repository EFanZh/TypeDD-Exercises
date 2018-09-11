total myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in plusSuccRightSucc m k
