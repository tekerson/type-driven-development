import Fuel

%default total

data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

greet : RunIO ()
greet = do
  putStr "Name: "
  name <- getLine
  if name == ""
    then do putStrLn "Bye!"
            Quit ()
    else do
      putStrLn ("Hello " ++ name)
      greet

run : Fuel -> RunIO a -> IO (Maybe a)
run _ (Quit value) = pure (Just value)
run (More fuel) (Do k f) = do
  res <- k
  run fuel (f res)
run Dry _ = pure Nothing

partial
main : IO ()
main = do run forever greet
          pure ()
