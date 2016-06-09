import Data.Vect

read_vect_len : (len : Nat) -> IO (Vect len String)
read_vect_len Z = pure []
read_vect_len (S k) = do
  x <- getLine
  xs <- read_vect_len k
  pure (x :: xs)

read_vect : IO (n ** Vect n String)
read_vect = do
  x <- getLine
  if (x == "")
    then pure (_ ** [])
    else do (_ ** xs) <- read_vect
            pure (_ ** (x :: xs))

zipInputs : IO ()
zipInputs = do
  putStrLn "1:"
  (len1 ** vec1) <- read_vect
  putStrLn "2:"
  (len2 ** vec2) <- read_vect
  case exactLength len1 vec2 of
    Nothing => putStrLn "Input lengths must be equal"
    Just vec2' => printLn (zip vec1 vec2')
