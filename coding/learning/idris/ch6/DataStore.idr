module Main

import Data.Vect

data Schema

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString   = String
SchemaType SInt      = Int
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
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store 
  = let storeItems = items store in
        case integerToFin pos (size store) of
             Nothing => Just ("out of range\n", store)
             (Just id) => Just (display (index id (items store)) ++ "\n", store)

data Command : Schema -> Type where
  Add    : SchemaType schema -> Command schema
  Get    : Integer -> Command schema
  Size   : Command schema
  Search : String -> Command schema
  Quit   : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input   = ?parsePrefix_rhs_1
parsePrefix SInt input      
  = case span isDigit input of
        ("", rest) => Nothing
        (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input 
  = case parsePrefix schemal input of
         Nothing => Nothing
         (Just (l_val, input')) => (case parsePrefix schemar input' of
                                         Nothing => Nothing
                                         (Just (_rval, input'')) => Just ((l_val, r_val), input''))

parseBySchema : (schema : Schema) -> (str : String) -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Just (res, "") => Just res
                                  (Just _) => Nothing
                                  Nothing => Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" str = case parseBySchema schema str of
                                     Nothing => Nothing
                                     (Just item) => Just (Add item)
parseCommand schema "get" val = case all isDigit (unpack val) of
                                     False => Nothing
                                     True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "size" "" = Just Size
parseCommand schema "search" str = Just (Search str)
parseCommand _ _ _ = Nothing                              

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input
  = case span (/= ' ') input of
         (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input 
  = case parse (schema store) input of
         Nothing => Just ("Invalid command\n", store)
         Just (Add item) => 
           Just ("ID " ++ show (size store) ++ "\n", addToStore store (?convert item))
         Just (Get pos) => getEntry pos store
         Just Size => Just ("" ++ show (size store) ++ " items\n", store)
         Just (Search str) => ?searchEntries -- str store
         Just Quit => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput

{-
searchEntries : (str : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchEntries str store = let storeItems = items store
                              matchingItems = filterEntries Z storeItems in
                              Just ("\n" ++ matchingItems, store)
  where
    -- how can we access {n} ? seems not to work
    filterEntries : Nat -> Vect n String -> String
    filterEntries _ [] = "\n"
    filterEntries idx (s :: ss) 
      = case Strings.isInfixOf str s of
             False => filterEntries (S idx) ss
             True => show idx ++ ": " ++ s ++ "\n" ++ filterEntries (S idx) ss
    

-}