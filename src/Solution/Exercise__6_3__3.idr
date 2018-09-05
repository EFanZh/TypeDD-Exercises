import Data.Vect

infixr 5 .+.

data Schema = SChar | SString | SInt | (.+.) Schema Schema

total SchemaType : Schema -> Type
SchemaType SChar = Char
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
    constructor MkData
    schema : Schema
    size : Nat
    items : Vect size (SchemaType schema)

data Command : Schema -> Type where
    SetSchema : (newschema : Schema) -> Command schema
    Add : SchemaType schema -> Command schema
    GetAll : Command schema
    Get : Integer -> Command schema
    Quit : Command schema

total parseSchema : List String -> Maybe Schema
parseSchema ("Char" :: xs) = case xs of
                                 [] => Just SChar
                                 _ => do xs_sch <- parseSchema xs
                                         Just (SChar .+. xs_sch)
parseSchema ("String" :: xs) = case xs of
                                   [] => Just SString
                                   _ => do xs_sch <- parseSchema xs
                                           Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) = case xs of
                                [] => Just SInt
                                _ => do xs_sch <- parseSchema xs
                                        Just (SInt .+. xs_sch)
parseSchema _ = Nothing

total parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SChar input = case span (\c => not (isSpace c)) input of
                              (first, rest) => case unpack first of
                                                   ch :: [] => Just (ch, ltrim rest)
                                                   _ => Nothing
parsePrefix SString input = getQuoted (unpack input) where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                             ("", rest) => Nothing
                             (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input = do (l_val, input') <- parsePrefix schemal input
                                             (r_val, input'') <- parsePrefix schemar input'
                                             Just ((l_val, r_val), input'')

total parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = do (res, "") <- parsePrefix schema input | _ => Nothing
                                Just res

total parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest = do restok <- parseBySchema schema rest
                                    Just (Add restok)
parseCommand schema "get" "" = Just GetAll
parseCommand schema "get" val = case all isDigit (unpack val) of
                                    False => Nothing
                                    True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest = do schemaok <- parseSchema (words rest)
                                       Just (SetSchema schemaok)
parseCommand _ _ _ = Nothing

total parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                         (cmd, args) => parseCommand schema cmd (ltrim args)

total addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = MkData schema _ (addToData store) where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema)
    addToData [] = [newitem]
    addToData (item :: items) = item :: addToData items

total setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                             Z => Just (MkData schema _ [])
                             S k => Nothing

total display : SchemaType schema -> String
display {schema = SChar} item = show item
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

total getAll : DataStore -> Maybe (String, DataStore)
getAll store = loop "" Z (items store) where
    loop : String -> Nat -> Vect size (SchemaType schema) -> Maybe (String, DataStore)
    loop acc k [] = Just (acc, store)
    loop acc k (x :: xs) = loop (acc ++ show k ++ ": " ++ display x ++ "\n") (S k) xs

total getEntry : (pos : Integer) -> (store : DataStore) ->  Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
    case integerToFin pos (size store) of
        Nothing => Just ("Out of range\n", store)
        Just id => Just (display (index id (items store)) ++ "\n", store)

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                               Nothing => Just ("Invalid command\n", store)
                               Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                               Just (SetSchema schema') => case setSchema store schema' of
                                                               Nothing => Just ("Can't update schema\n", store)
                                                               Just store' => Just ("OK\n", store')
                               Just GetAll => getAll store
                               Just (Get pos) => getEntry pos store
                               Just Quit => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
