readToBlank : IO (List String)
readToBlank = loop [] where
    loop : List String -> IO (List String)
    loop acc = do putStr "Input: "
                  input <- getLine
                  if input == "" then pure (reverse acc)
                                 else loop (input :: acc)

total writeLinesToFile : String -> List String -> IO ()
writeLinesToFile path inputs = let content = (foldl (++) "" (map (++ "\n") inputs))
                               in do Right _ <- writeFile path content | Left error => printLn error
                                     pure ()

readAndSave : IO ()
readAndSave = loop [] where
    loop : List String -> IO ()
    loop acc = do putStr "Input: "
                  input <- getLine
                  if input == ""
                  then do putStr "File path: "
                          filePath <- getLine
                          writeLinesToFile filePath (reverse acc)
                  else loop (input :: acc)
