import Data.Vect

readToBlank : IO (List String)
readToBlank = do
  line <- getLine
  if (line == "")
    then pure []
    else do
      lines <- readToBlank
      pure (line :: lines)

readAndSave : IO ()
readAndSave = do
  content <- readToBlank
  filename <- getLine
  Right _ <- (writeFile filename (unlines content))
    | Left err => (putStrLn ("ERR" ++ show err))
  pure ()

readLinesFile : (file : File) -> IO (Either FileError (n ** Vect n String))
readLinesFile file = do
  eof <- fEOF file
  if eof
    then pure (Right (_ ** []))
    else do
      Right line <- fGetLine file
        | Left err => pure (Left err)
      Right (n ** lines) <- readLinesFile file
        | Left err => pure (Left err)
      pure (Right (_ ** line :: lines))

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right file <- openFile filename Read
    | Left _ => pure (_ ** [])
  Right vec <- readLinesFile file
    | Left _ => pure (_ ** [])
  pure vec
