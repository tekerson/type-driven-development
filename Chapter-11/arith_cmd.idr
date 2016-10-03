import System

import Command
import Arith
import Fuel

%default total

mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score
    = do PutStr "Correct!\n"
         quiz nums (score + 1)

  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums answer score
    = do PutStr ("Wrong, the answer is " ++ show answer ++ "\n")
         quiz nums score

  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) score
    = do PutStr ("Score: " ++ show score ++ "\n")
         PutStr (show num1 ++ " * " ++ show num2 ++ " ")
         answer <- GetLine
         if toLower answer == "quit" then Quit score else
           if (cast answer == num1 * num2)
           then correct nums score
           else wrong nums (num1 * num2) score

partial
main : IO ()
main = do
  seed <- time
  Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
       | Nothing => putStrLn "Out Of Fuel"
  putStrLn ("Final Score: " ++ show score)
