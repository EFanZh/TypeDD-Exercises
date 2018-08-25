total printLonger : IO ()
printLonger = do putStr "First string: " >>=
                  \_ => getLine >>=
                  \str1 => putStr "Second string: " >>=
                  \_ => getLine >>=
                  \str2 => let len1 = length str1
                               len2 = length str2
                           in putStrLn (show (max len1 len2))
