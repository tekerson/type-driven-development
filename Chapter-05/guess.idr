module Main

import System

readNum : IO (Maybe Nat)
readNum = do
  input <- getLine
  let output =  if all isDigit (unpack input)
    then (Just (cast input))
    else Nothing
  pure output

takeGuess : (target : Nat) -> IO (Maybe Ordering)
takeGuess target = do
  Just g <- readNum
    | Nothing => pure Nothing
  pure (Just (compare g target))

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStr ((show guesses) ++ ": Guess? ")
  result <- takeGuess target
  case result of
    Just EQ => putStrLn "Got it!"
    Nothing => putStrLn "Invalid Number" >>= \_ => guess target (S guesses)
    Just LT => putStrLn "Too Low" >>= \_ => guess target (S guesses)
    Just GT => putStrLn "Too High" >>= \_ => guess target (S guesses)

main : IO ()
main = do
  t <- time
  let target = mod t 100
  guess (cast target) Z
