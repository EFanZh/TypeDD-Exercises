import System

total readNumber : IO (Maybe Nat)
readNumber = do input <- getLine
                if all isDigit (unpack input)
                then pure (Just (cast input))
                else pure Nothing

guess : (target : Nat) -> IO ()
guess target = do putStr "Guess the number: "
                  Just guessed <- readNumber | Nothing => do putStrLn "Not a number."
                                                             guess target
                  case compare guessed target of
                       LT => do putStrLn "Too low."
                                guess target
                       EQ => do putStrLn "Correct."
                                pure ()
                       GT => do putStrLn "Too high."
                                guess target

random100 : IO Nat
random100 = do now <- time
               pure (S (cast (mod now 100)))

main : IO ()
main = random100 >>= guess
