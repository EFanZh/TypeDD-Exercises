total readNumber : IO (Maybe Nat)
readNumber = do input <- getLine
                if all isDigit (unpack input)
                then pure (Just (cast input))
                else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do putStr ("Guess the number (" ++ (show guesses) ++ " tries): ")
                          Just guessed <- readNumber | Nothing => do putStrLn "Not a number."
                                                                     guess target (S guesses)
                          case compare guessed target of
                               LT => do putStrLn "Too low."
                                        guess target (S guesses)
                               EQ => do putStrLn "Correct."
                                        pure ()
                               GT => do putStrLn "Too high."
                                        guess target (S guesses)
