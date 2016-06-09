module Main

word_lengths : List String -> List Nat
word_lengths [] = []
word_lengths (word :: words) = length word :: word_lengths words

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k
