palindrome: String -> Bool
palindrome s = s == toLower (reverse s)
