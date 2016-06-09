
printLength : IO ()
printLength =
  putStr "Input: " >>= \_ =>
  getLine >>= \input =>
   let len = length input in
             putStrLn (show len)

printLength' : IO ()
printLength' = do
  putStr "Input: "
  input <- getLine
  let len = length input
  putStrLn (show len)

longest : IO ()
longest = do
  putStr "1: "
  in1 <- getLine
  putStr "2: "
  in2 <- getLine
  let len1 = length in1
  let len2 = length in2
  let longest = if len2 > len1 then in1 else in2
  putStrLn ("Longest: " ++ longest)

readNum : IO (Maybe Nat)
readNum = do
  input <- getLine
  let output =  if all isDigit (unpack input)
    then (Just (cast input))
    else Nothing
  pure output
