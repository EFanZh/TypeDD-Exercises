import Data.Vect

data DataStore : Type where
    MkData : (size : Nat) -> (items : Vect size String) -> DataStore

total size : DataStore -> Nat
size (MkData size' items') = size'

total items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

total addToStore : DataStore -> String -> DataStore
addToStore (MkData size store) newitem = MkData _ (addToData store) where
    addToData : Vect oldsize String -> Vect (S oldsize) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String | Get Integer | Size | Quit

total parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
    False => Nothing
    True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand _ _ = Nothing

total parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
    (cmd, args) => parseCommand cmd (ltrim args)

total getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
    case integerToFin pos (size store) of
        Nothing => Just ("Out of range\n", store)
        Just id => Just (index id (items store) ++ "\n", store)

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (Get pos) => getEntry pos store
    Just Size => Just (show (size store) ++ "\n", store)
    Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
