module Main

import Average
import Palindrome

counts : String -> (Nat, Nat)
counts s = (length (words s), length s)

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length n strs = length (filter (\w => length w > n) strs)

describe : String -> String
describe str = "The average word length is: "
  ++ show (average str) ++ ".\n"
  ++ "It " ++ (if palindrome 10 str then "is" else "is not") ++ " a palindrome.\n"

main : IO ()
main = repl "Enter a string:"
            describe
