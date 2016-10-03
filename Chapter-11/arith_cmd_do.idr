import System
import Fuel
import Arith

import Command

data Input = Answer Int
           | QuitCmd

data Score = MkScore Nat Nat

Show Score where
  show (MkScore y n) = (show y ++ " / " ++ show (y + n))

addCorrect : Score -> Score
addCorrect (MkScore k j) = MkScore (k + 1) j

addWrong : Score -> Score
addWrong (MkScore k j) = MkScore k (j + 1)

readInput : (prompt : String) -> Command Input
readInput prompt = do
  PutStr prompt
  answer <- GetLine
  if toLower answer == "quit"
    then Pure QuitCmd
    else Pure (Answer (cast answer))

mutual
  correct : Stream Int -> (score : Score) -> ConsoleIO Score
  correct nums score
    = do PutStr "Correct!\n"
         quiz nums (addCorrect score)

  wrong : Stream Int -> Int -> (score : Score) -> ConsoleIO Score
  wrong nums answer score
    = do PutStr ("Wrong, the answer is " ++ show answer ++ "\n")
         quiz nums (addWrong score)

  quiz : Stream Int -> (score : Score) -> ConsoleIO Score
  quiz (num1 :: num2 :: nums) score
    = do PutStr ("Score: " ++ show score ++ "\n")
         input <- readInput (show num1 ++ " * " ++ show num2 ++ " ")
         case input of
           Answer answer => if (answer == num1 * num2)
                             then correct nums score
                             else wrong nums (num1 * num2) score
           QuitCmd => Quit score

partial
main : IO ()
main = do
  seed <- time
  Just score <- run forever (quiz (arithInputs (fromInteger seed)) (MkScore 0 0))
       | Nothing => putStrLn "Out Of Fuel"

  putStrLn ("Final Score: " ++ show score)
