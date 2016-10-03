module Command

import Fuel

public export
data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String
  ReadFile : String -> Command String
  WriteFile : String -> String -> Command ()
  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

public export
data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  public export
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

namespace ConsoleDo
  public export
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

public export
runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile file) =
  (either (const "") id) <$> (readFile file)
runCommand (WriteFile file content) =
  (either (const ()) id) <$> (writeFile file content)
runCommand (Pure x) = pure x
runCommand (Bind c f) = do
  res  <- runCommand c
  runCommand (f res)

public export
run : Fuel -> ConsoleIO a -> IO (Maybe a)
run _ (Quit value) = pure (Just value)
run (More fuel) (Do k f) = do
  res <- runCommand k
  run fuel (f res)
run Dry _ = pure Nothing
