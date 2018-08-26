import Data.Vect

readAllLines : File -> (n ** Vect n String) -> IO (n ** Vect n String)
readAllLines file (n ** xs) = do eof <- fEOF file
                                 if eof then pure (n ** reverse xs)
                                        else do Right line <- fGetLine file | Left error => do printLn error
                                                                                               pure (0 ** [])
                                                readAllLines file (S n ** line :: xs)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right file <- openFile filename Read | Left error => do printLn error
                                                                                   pure (0 ** [])
                           result <- readAllLines file (0 ** [])
                           closeFile file
                           pure result
