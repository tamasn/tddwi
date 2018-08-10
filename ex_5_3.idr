import Data.Vect

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if x == ""
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)

readAndSave : IO ()
readAndSave = do x <- readString 
                 fn <- getLine
                 saveString fn x
  where
    readString : IO String
    readString = do x <- getLine
                    if x == ""
                       then pure ""
                       else do rest <- readString
                               pure (x ++ "\n" ++ rest)
    saveString : (file : String) -> (content: String) -> IO ()
    saveString file content = 
      do
        Right _ <- writeFile file content
          | Left err => putStrLn (show err)
        pure ()

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename =
  do
    Right f <- openFile filename Read
      | Left err => do
                      putStrLn (show err)
                      pure (_ ** [])
    Right content <- readFile f
      | Left err => do
                      putStrLn (show err)
                      pure (_ ** [])
    pure content
  where
    readFile : (fn : File) -> IO (Either FileError (n ** Vect n String))
    readFile fn = do eof <- fEOF fn
                     case eof of
                          True => pure (Right (_ ** []))
                          False => do
                                          Right line <- fGetLine fn
                                            | Left err => pure (Left err)
                                          Right (_ ** rest) <- readFile fn
                                            | Left err => pure (Left err)
                                          pure (Right (_ ** (line :: rest)))

