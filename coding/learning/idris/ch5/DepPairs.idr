import Data.Vect

readVect : IO (len ** Vect len String)
readVect = do
  x <- getLine
  if (x == "")
    then pure (_ ** [])
    else do 
      (_ ** xs) <- readVect
      pure (_ ** (x :: xs))

zipInputs : IO ()
zipInputs = do
  putStrLn "Enter first vector (blank line to end):"
  (len1 ** vect1) <- readVect
  putStrLn "Enter second vector (blank line to end):"
  (len2 ** vect2) <- readVect
  (case exactLength len1 vect2 of
        Nothing => putStrLn "Vectors are different lengths"
        (Just vect2') => printLn (zip vect1 vect2'))
