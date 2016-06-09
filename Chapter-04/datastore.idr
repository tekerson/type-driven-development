module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

search : (store : DataStore) -> String -> List String
search (MkData _ items) q = filter (isInfixOf q) (toList items)

search' : (store : DataStore) -> String -> List (Nat, String)
search' (MkData size items) q
  = filter ((isInfixOf q) . snd)
           (zip (enumFromTo 0 size) (toList items))

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) item = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [item]
    addToData (x :: xs) = x :: addToData xs

sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs sum input
  = let val = cast input in
        if val < 0
        then Nothing
        else let newVal = sum + val in
                 Just ("Subtotal: " ++ show newVal ++ "\n", newVal)

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" query = Just (Search query)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (store : DataStore) -> (id : Integer) -> Maybe String
getEntry store id = let items = items store in
                    flip index items <$> integerToFin id (size store)

processCommand : (store : DataStore) -> (cmd : Command) -> Maybe (String, DataStore)
processCommand store (Add item)
  = Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
processCommand store (Get id) = case getEntry store id of
                                     Just item => Just (item ++ "\n", store)
                                     Nothing => Just ("Out of range\n", store)
processCommand store (Search query)
  = let results = search' store query
        results' = map (\(k, v) => show k ++ ": " ++ v) results in
        Just (unlines results' ++ "\n", store)
processCommand store Size = Just ("Size: " ++ show (size store) ++ "\n", store)
processCommand store Quit = Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
  = case parse input of
         Nothing => Just ("Invalid command\n", store)
         Just cmd => processCommand store cmd

main : IO ()
main = replWith (MkData _ [])  "Command: " processInput
