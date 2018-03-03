import Data.Vect

readToBlank : IO (List String)
readToBlank = do
  x <- getLine
  if (x == "")
    then pure []
    else do 
      xs <- readToBlank
      pure (x :: xs)

readAndSave : IO ()
readAndSave = do
    ss        <- readToBlank
    putStrLn "Enter filename: "
    fileName  <- getLine
    fe        <- openFile fileName WriteTruncate
    either 
      (\e => putStrLn ("FileError: " ++ show e))
      (\f => writeListToFile ss f >>= \_ => closeFile	f)
      fe

  where
    writeListToFile : List String -> File -> IO ()
    writeListToFile [] f = pure ()
    writeListToFile (x :: xs) f = do
      fPutStrLn f x
      writeListToFile xs f

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
    fe <- openFile filename Read
    either 
        (\e => do
          putStrLn ("Couldn't open file: " ++ show e)
          pure (_ ** []))
        (\f => do
          v <- readVect f
          closeFile	f
          pure v)
        fe
  where
    readVect : File -> IO (n ** Vect n String)
    readVect f = do
      eof <- fEOF f 
      if eof
        then pure (_ ** [])
        else do
          fe <- fGetLine f
          either 
            (\e => do
              putStrLn ("Couldn't read from file: " ++ show e)
              pure (_ ** []))
            (\x => do
              (_ ** xs) <- readVect f
              pure (_ ** (x :: xs)))
            fe