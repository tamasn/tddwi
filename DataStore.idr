module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
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
               SetSchema : (newschema : Schema) -> Command schema
               Add : SchemaType schema -> Command schema
               Get : Integer -> Command schema
               GetAll : Command schema
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
parsePrefix SChar x = case (unpack x) of
                           [] => Nothing
                           (x :: xs) => Just (x, ltrim(pack xs))
parsePrefix (schemal .+. schemar) input = 
  do
    (l_val, input') <- parsePrefix schemal input 
    (r_val, input'') <- parsePrefix schemar input'
    Just ((l_val, r_val), input'')

mutual
  parseSchemaItem : (schemaType : Schema) -> (rest : List String) -> Maybe Schema
  parseSchemaItem schemaType [] = Just schemaType
  parseSchemaItem schemaType rest =
    do xs_sch <- parseSchema rest 
       Just (schemaType .+. xs_sch)

  parseSchema : List String -> Maybe Schema
  parseSchema ("String" :: xs) = parseSchemaItem SString xs
  parseSchema ("Int" :: xs) = parseSchemaItem SInt xs
  parseSchema ("Char" :: xs) = parseSchemaItem SChar xs
  parseSchema _ = Nothing

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema x = case parsePrefix schema x of
                              Nothing => Nothing
                              (Just (a, "")) => Just a
                              (Just (a, b)) => Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" str = do scm <- parseBySchema schema str 
                                   Just (Add scm)
parseCommand schema "schema" str = do x <- parseSchema (words str) 
                                      Just (SetSchema x)
parseCommand schema "get" "" = Just GetAll
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
display {schema = SChar} x = show x
display {schema = (y .+. z)} (a, b) = display a ++ ", " ++ display b

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = 
  let store_items = items store in
      case integerToFin pos (size store) of
          Nothing => Just ("Out of range\n", store)
          Just id => Just (display (index id store_items) ++ "\n", store)

getAllEntries : (store : DataStore) -> String
getAllEntries store = printItems (items store) 0
  where
    printItems : (items : Vect _ (SchemaType schema)) -> (i : Nat) -> String
    printItems [] _ = ""
    printItems (x :: xs) i = (show i) ++ ": " ++ (display x) ++ "\n" ++ (printItems xs (i + 1))

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              (S k) => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse (schema store) inp of
                              Nothing => Just ("Invalid command\n", store)
                              (Just (Add item)) =>
                                Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              Just (SetSchema schema') =>
                                case setSchema store schema' of
                                      Nothing => Just ("Can't update schema\n", store)
                                      (Just x) => Just ("OK\n", x)
                              (Just (Get pos)) => getEntry pos store 
                              (Just (GetAll)) => Just (getAllEntries store, store) 
                              (Just Quit) => Nothing

main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
