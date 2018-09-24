%default total

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String
    ReadFile : (filepath : String) -> Command (Either FileError String)
    WriteFile : (filepath : String) -> (contents : String) -> Command (Either FileError ())
    Pure : ty -> Command ty
    Bind : Command a -> (a -> Command b) -> Command b

namespace CommandDo
    (>>=) : Command a -> (a -> Command b) -> Command b
    (>>=) = Bind

data ConsoleIO : Type -> Type where
    Quit : a -> ConsoleIO a
    Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace ConsoleDo
    (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
    (>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile filepath) = readFile filepath
runCommand (WriteFile filepath contents) = writeFile filepath contents
runCommand (Pure val) = pure val
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = do pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry p = pure Nothing
