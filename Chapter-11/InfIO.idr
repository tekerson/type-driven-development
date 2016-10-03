import Fuel

%default total

public export
data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO

public export
(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

public export
run : Fuel -> InfIO -> IO ()
run (More fuel) (Do c f) = do
  res <- c
  run fuel (f res)
run Dry _ = putStrLn "Out of Fuel"
