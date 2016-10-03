import InfIO

%default total

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do
  putStr prompt
  cmd <- getLine
  putStr (action cmd)
  totalREPL prompt action

partial
main : IO ()
main = run forever (totalREPL "\n: " toUpper)
