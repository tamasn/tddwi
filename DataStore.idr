module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
       constructor MkData 
       schema : Schema
       size : Nat
       items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newitem  = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command : Schema -> Type where
               Add : SchemaType schema -> Command schema
               Get : Integer -> Command schema
               Quit : Command schema 

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString x = getQuoted (unpack x)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                 _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt x = case span isDigit x of
                          ("", b) => Nothing 
                          (a, b) => Just (cast a, ltrim b)
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               Nothing => Nothing
                                               (Just (l_val, input')) => 
                                                      case parsePrefix schemar input' of
                                                            Nothing => Nothing
                                                            (Just (r_val, input'')) =>
                                                                  Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema x = case parsePrefix schema x of
                              Nothing => Nothing
                              (Just (a, "")) => Just a
                              (Just (a, b)) => Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" str = case parseBySchema schema str of
                                     Nothing => Nothing
                                     Just scm => Just (Add scm)
parseCommand schema "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args) 

display : SchemaType schema -> String
display {schema = SString} x = show x
display {schema = SInt} x = show x
display {schema = (y .+. z)} (a, b) = display a ++ ", " ++ display b

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = 
  let store_items = items store in
      case integerToFin pos (size store) of
          Nothing => Just ("Out of range\n", store)
          Just id => Just (display (index id store_items), store)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
                              Nothing => Just ("Invalid command\n", store)
                              (Just (Add item)) =>
                                Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              (Just (Get pos)) => getEntry pos store 
                              (Just Quit) => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
