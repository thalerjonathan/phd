module Main

import Data.Vect

data DataStore : Type where
     MkData : (size  : Nat) ->
              (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand _ _ = Nothing                              

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let storeItems = items store in
                             (case integerToFin pos (size store) of
                                   Nothing => Just ("out of range\n", store)
                                   (Just id) => Just (index id storeItems ++ "\n", store))

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
    
total
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp 
  = case parse inp of
        Nothing => Just ("Invalid command\n", store)
        Just (Add item) => let itemId = size store
                               store' = addToStore store item in
                               Just ("ID " ++ show itemId ++ "\n", store')
        Just (Get pos) => getEntry pos store
        Just Size => Just ("" ++ show (size store) ++ " items\n", store)
        Just (Search str) => searchEntries str store
        Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput