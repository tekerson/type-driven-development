StringOrInt : (x : Bool) -> Type
StringOrInt False = String
StringOrInt True = Int

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False str = trim str
valToString True n = cast n

valToString' : (isInt : Bool) -> (case isInt of
                                       False => String
                                       True => Int) -> String
valToString' False x = trim x
valToString' True x = cast x
