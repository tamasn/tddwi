module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> 
           (items : Vect size String) ->
           DataStore
size : DataStore -> Nat
size (MkData size' items) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem  = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
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
parseCommand "size" "" = Just Size
parseCommand "search" str = Just (Search str)
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args) 

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = 
  let store_items = items store in
      case integerToFin pos (size store) of
          Nothing => Just ("Out of range\n", store)
          Just id => Just (index id store_items ++ "\n", store)

searchText : (text : String) -> (store : DataStore) -> String
searchText text store = let xs = items store
                            results = srch text xs 0 in
                            results
                        where
                          srch : String -> Vect n String -> Integer -> String
                          srch _ [] _ = ""
                          srch term (y :: xs) i
                            = let result: String = if isInfixOf term y then (cast i) ++ ": " ++ y ++ "\n" else "" in
                                  result ++ (srch term xs (i + 1))

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              (Just (Add item)) =>
                                Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              (Just (Get pos)) => getEntry pos store 
                              (Just Size) => Just ("Size of the store: " ++ (cast (size store)) ++ "\n", store)
                              (Just (Search text)) => Just (searchText text store, store)
                              (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
