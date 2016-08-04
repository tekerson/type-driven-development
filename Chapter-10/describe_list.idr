import ListLast

describe_help : (Show a) => (input : List a) -> (form : ListLast input) -> String
describe_help [] Empty = "Empty"
describe_help (xs ++ [x]) (NonEmpty xs x)
  = "Non-empty, initial portion = " ++ show xs

describe_list_end : Show a => List a -> String
describe_list_end xs = describe_help xs (listLast xs)

describe_list_end' : Show a => List a -> String
describe_list_end' xs with (listLast xs)
  describe_list_end' [] | Empty = "Empty"
  describe_list_end' (xs' ++ [x]) | (NonEmpty xs' x)
    = "Non-empty, initial portion = " ++ show xs'
