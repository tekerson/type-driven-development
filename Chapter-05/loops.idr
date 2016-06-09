module Main

import System

countdown : (secs : Nat) -> IO ()
countdown Z = putStrLn "Lift Off!"
countdown t@(S s) = do
  putStrLn (show t)
  usleep 1000000
  countdown s
