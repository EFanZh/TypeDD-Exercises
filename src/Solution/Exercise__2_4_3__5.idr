-- I don’t think this function should be called “palindrome” any more.
palindrome: Nat -> String -> Bool
palindrome n s = (length s) > n
