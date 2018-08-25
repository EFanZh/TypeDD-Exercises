myRepl : (prompt : String) -> (onInput : String -> String) -> IO ()
myRepl prompt onInput = do putStr prompt
                           input <- getLine
                           putStr (onInput input)
                           myRepl prompt onInput

myReplWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt onInput = do putStr prompt
                                     input <- getLine
                                     case onInput state input of
                                         Nothing => pure ()
                                         Just (output, newState) => do putStr output
                                                                       myReplWith newState prompt onInput
