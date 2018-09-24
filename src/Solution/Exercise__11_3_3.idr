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

------------------------------------------------------------------------------------------------------------------------

data ShellCommand : Type where
    Cat : (filepath : String) -> ShellCommand
    Copy : (source : String) -> (destination : String) -> ShellCommand
    Exit : ShellCommand

parseShellCommand : String -> Maybe ShellCommand
parseShellCommand input
    = case words input of
          ["cat", filepath] => Just (Cat filepath)
          ["copy", source, destination] => Just (Copy source destination)
          ["exit"] => Just Exit
          _ => Nothing

showError : Show a => a -> Command ()
showError error = PutStr (show error ++ "\n")

makeCatCommand : (filepath : String) -> Command ()
makeCatCommand filepath = do Right content <- ReadFile filepath | Left error => showError error
                             PutStr content

makeCopyCommand : (source : String) -> (destination : String) -> Command ()
makeCopyCommand source destination = do Right content <- ReadFile source | Left error => showError error
                                        Right _ <- WriteFile destination content | Left error => showError error
                                        Pure ()

mutual
    doPrompt : Command (Maybe ShellCommand)
    doPrompt = do PutStr "$ "
                  input <- GetLine
                  Pure (parseShellCommand input)

    doCat : (filepath : String) -> ConsoleIO ()
    doCat filepath = do makeCatCommand filepath
                        shell

    doCopy : (source : String) -> (destination : String) -> ConsoleIO ()
    doCopy source destination = do makeCopyCommand source destination
                                   shell

    doExit : ConsoleIO ()
    doExit = Quit ()

    shell : ConsoleIO ()
    shell = do Just command <- doPrompt | Nothing => do PutStr "Invalid command.\n"
                                                        shell
               case command of
                   Cat filepath => doCat filepath
                   Copy source destination => doCopy source destination
                   Exit => doExit

partial main : IO ()
main = do run forever shell
          pure ()
