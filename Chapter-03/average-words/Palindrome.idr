module Palindrome

export
palindrome : Nat -> String -> Bool
palindrome minLen str = longEnough && lowered == (Strings.reverse lowered)
  where
    longEnough = length str > minLen
    lowered = toLower str
