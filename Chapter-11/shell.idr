import Command

readCmd : String -> Either String (Command a)
readCmd input = case words input of
  [] => Left "No Command"
  (cmd :: args) => Left ("Unknown Command " ++ cmd)

cmd : ConsoleIO ()
cmd = do
  PutStr "$ \n"
  input <- GetLine
  case readCmd input of
    Left err => PutStr err
    Right _ => PutStr "OK"
  cmd


-- partial
-- main : IO ()
-- main = do
--   Just score <- run forever (quiz (arithInputs (fromInteger seed)) (MkScore 0 0))
--        | Nothing => putStrLn "Out Of Fuel"

--   putStrLn ("Final Score: " ++ show score)
