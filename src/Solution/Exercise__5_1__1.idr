total printLonger : IO ()
printLonger = do putStr "First string: "
                 str1 <- getLine
                 putStr "Second string: "
                 str2 <- getLine
                 let len1 = length str1
                 let len2 = length str2
                 putStrLn (show (max len1 len2))
