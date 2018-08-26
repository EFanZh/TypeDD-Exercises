readToBlank : IO (List String)
readToBlank = loop [] where
    loop : List String -> IO (List String)
    loop acc = do putStr "Input: "
                  input <- getLine
                  if input == "" then pure (reverse acc)
                                 else loop (input :: acc)
