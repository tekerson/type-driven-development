import Data.Primitives.Views
import ArithInputs

quiz : Stream Int -> (score : Nat) -> IO ()
quiz (num1 :: num2 :: nums) score
  = do putStrLn ("Score: " ++ show score)
       putStr (show num1 ++ " * " ++ show num2 ++ " ")
       answer <- getLine
       if (cast answer == num1 * num2)
       then do putStrLn "Correct!"
               quiz nums (score + 1)
       else do putStrLn ("Wrong, the answer is " ++ show (num1 * num2))
               quiz nums score
