module Main

import Data.Vect

data Schema

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString   = String
SchemaType SInt      = Int
SchemaType SChar     = Char
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData
       schema : Schema
       size   : Nat
       items  : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newItem 
           = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData []        = [newItem]
    addToData (x :: xs) = x :: addToData xs

display : SchemaType schema -> String
display {schema = SString} item             = show item
display {schema = SInt} item                = show item
display {schema = SChar} item               = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store 
  = let storeItems = items store in
        case integerToFin pos (size store) of
             Nothing => Just ("out of range\n", store)
             Just id => Just (display (index id (items store)) ++ "\n", store)

data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add       : SchemaType schema -> Command schema
  Get       : Integer -> Command schema
  GetAll    : Command schema
  Size      : Command schema
  Quit      : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) 
      = case span (/= '"') xs of
             (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
             _ => Nothing
    getQuoted _ = Nothing

parsePrefix SInt input      
  = case span isDigit input of
        ("", rest)  => Nothing
        (num, rest) => Just (cast num, ltrim rest)

parsePrefix SChar input = getSingleQuoted (unpack input)
  where
    getSingleQuoted : List Char -> Maybe (Char, String)
    getSingleQuoted (c :: xs) = Just (c, ltrim (pack xs))
    getSingleQuoted _ = Nothing

parsePrefix (schemal .+. schemar) input 
  = case parsePrefix schemal input of
         Nothing              => Nothing
         Just (l_val, input') => case parsePrefix schemar input' of
                                      Nothing               => Nothing
                                      Just (r_val, input'') => Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> (str : String) -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  Just _         => Nothing
                                  Nothing        => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) 
  = case xs of
         [] => Just SString
         _  => case parseSchema xs of
                    Nothing     => Nothing
                    Just xs_sch => Just (SString .+. xs_sch)

parseSchema ("Int" :: xs)
  = case xs of
         [] => Just SInt
         _  => case parseSchema xs of
                    Nothing     => Nothing
                    Just xs_sch => Just (SInt .+. xs_sch)

parseSchema ("Char" :: xs)
  = case xs of
         [] => Just SChar
         _  => case parseSchema xs of
                    Nothing     => Nothing
                    Just xs_sch => Just (SChar .+. xs_sch)

parseSchema _ = Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "schema" str = do
  schemaOk <- parseSchema (words str)
  Just (SetSchema schemaOk)
parseCommand schema "add" str 
  = case parseBySchema schema str of
         Nothing    => Nothing
         Just item  => Just (Add item)
parseCommand schema "get" "" = Just GetAll
parseCommand schema "get" val
  = case all isDigit (unpack val) of
         False => Nothing
         True  => Just (Get (cast val))
parseCommand schema "quit" ""     = Just Quit
parseCommand schema "size" ""     = Just Size
parseCommand _      _        _    = Nothing                              

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input
  = case span (/= ' ') input of
         (cmd, args) => parseCommand schema cmd (ltrim args)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema 
  = case size store of
         Z     => Just (MkData schema _ [])
         (S k) => Nothing

getAllEntries : (store : DataStore) -> String
getAllEntries store = getAllEntriesAux Z (items store)
  where
    getAllEntriesAux : Nat -> Vect size (SchemaType schema) -> String
    getAllEntriesAux k []         = "\n"
    getAllEntriesAux k (x :: xs)  = let idxStr = show k
                                        itemStr = display x in
                                        idxStr ++ ": " ++ itemStr ++ "\n" ++ getAllEntriesAux (S k) xs

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input 
  = case parse (schema store) input of
         Nothing            => Just ("Invalid command\n", store)
         Just (SetSchema s) => 
          case setSchema  store s of
               Nothing      => Just ("Can only update empty schema\n", store)
               Just store'  => Just ("OK\n", store')
         Just (Add item)    => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos)     => getEntry pos store
         Just GetAll        => 
          let entriesStr = getAllEntries store in
          Just (entriesStr, store)
         Just Size          => Just ("" ++ show (size store) ++ " items\n", store)
         Just Quit          => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput