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

-- search : (store : DataStore) -> String -> List String
-- search (MkData _ items) q = filter (isInfixOf q) (toList items)

-- search' : (store : DataStore) -> String -> List (Nat, String)
-- search' (MkData size items) q
--   = filter ((isInfixOf q) . snd)
--            (zip (enumFromTo 0 size) (toList items))

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) item = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [item]
    addToData (x :: xs) = x :: addToData xs

-- sumInputs : Integer -> String -> Maybe (String, Integer)
-- sumInputs sum input
--   = let val = cast input in
--         if val < 0
--         then Nothing
--         else let newVal = sum + val in
--                  Just ("Subtotal: " ++ show newVal ++ "\n", newVal)

data Command : Schema -> Type where
  SetSchema : (new : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  GetAll : Command schema
  Search : String -> Command schema
  Size : Command schema
  Quit : Command schema

mutual
  parseSchema : List String -> Maybe Schema
  parseSchema ("String" :: xs) = doParseSchema xs SString
  parseSchema ("Int" :: xs) = doParseSchema xs SInt
  parseSchema ("Char" :: xs) = doParseSchema xs SChar
  parseSchema _ = Nothing

  doParseSchema : List String -> Schema -> Maybe Schema
  doParseSchema [] type = Just type
  doParseSchema xs type = do
    xs' <- parseSchema xs
    pure (type .+. xs')

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema =
  case size store of
       Z => Just (MkData schema _ [])
       (S k) => Nothing

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) =
      case span (/= '"') xs of
        (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
        _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                             ("", rest) => Nothing
                             (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (Char, String)
    getQuoted ('\'' :: c :: '\'' :: rest) = Just (c, ltrim (pack rest))
    getQuoted ('\'' :: '\\' :: '\'' :: '\'' :: rest) = Just ('\'', ltrim (pack rest))
    getQuoted _ = Nothing
parsePrefix (lft .+. rgt) input = do
  (l, input') <- parsePrefix lft input
  (r, input'') <- parsePrefix rgt input'
  pure ((l, r), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = do
  (res, "") <- parsePrefix schema input | _ => Nothing
  pure res

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) -> Maybe (Command schema)
parseCommand schema "add" str = Add <$> parseBySchema schema str
parseCommand schema "get" "" = Just GetAll
parseCommand schema "get" val =
  case all isDigit (unpack val) of
       False => Nothing
       True => Just (Get (cast val))
parseCommand schema "search" query = Just (Search query)
parseCommand schema "size" "" = Just Size
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" str = SetSchema <$> parseSchema (words str)
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input =
  case span (/= ' ') input of
       (cmd, args) => parseCommand schema cmd (ltrim args)

display : (SchemaType schema) -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (_ .+. _)} (l, r) = (display l) ++ ", " ++ (display r)

showStore : DataStore -> String
showStore store = showItems (items store)
  where
    showItems : Vect size (SchemaType schema) -> String
    showItems = concat . map (\x => display x ++ "\n")

getEntry : (store : DataStore) -> (id : Integer) -> Maybe String
getEntry store id = do
  id' <- integerToFin id (size store)
  pure (display (index id' (items store)))

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
  = case parse (schema store) input of
         Nothing => Just ("Invalid command\n", store)
         Just cmd => processCommand cmd
  where
    processCommand : (cmd : Command (schema store)) -> Maybe (String, DataStore)
    processCommand (Add item)
      = Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    processCommand (Get id) = case getEntry store id of
                                         Just item => Just (item ++ "\n", store)
                                         Nothing => Just ("Out of range\n", store)
    processCommand GetAll = Just (showStore store, store)
    -- processCommand (Search query)
    --   = let results = search' store query
    --         results' = map (\(k, v) => show k ++ ": " ++ v) results in
    --         Just (unlines results' ++ "\n", store)
    processCommand Size = Just ("Size: " ++ show (size store) ++ "\n", store)
    processCommand (SetSchema schema') =
      case setSchema store schema' of
           Nothing => Just ("Can't update schema\n", store)
           Just store' => Just ("OK\n", store')
    processCommand Quit = Nothing

main : IO ()
main = replWith (MkData SString Z [])  "Command: " processInput
