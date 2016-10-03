import DataStore

test_store : DataStore (SString .+. SString .+. SInt)
test_store = addToStore ("Mercury", "Mariner 10", 1974) $
             addToStore ("Venus", "Venera", 1961) $
             addToStore ("Uranus", "Voyaget 2", 1986) $
             addToStore ("Uranus", "New Horizons", 2015) $
             empty

showItems : DataStore schema -> List (SchemaType schema)
showItems input with (storeView input)
  showItems input | SNil = []
  showItems (addToStore value store) | (SAdd rec)
    = value :: showItems store | rec

filterKeys : (test : SchemaType val_schema -> Bool) ->
             DataStore (SString .+. val_schema) -> List String
filterKeys test input with (storeView input)
  filterKeys test input | SNil = []
  filterKeys test (addToStore (key, value) store) | (SAdd rec)
    = if test value
      then key :: filterKeys test store | rec
      else filterKeys test store | rec

getValues : DataStore (SString .+. val_schema) ->
            List (SchemaType val_schema)
getValues input with (storeView input)
  getValues input | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec)
    = value :: getValues store | rec
