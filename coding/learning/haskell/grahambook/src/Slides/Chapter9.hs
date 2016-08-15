-- Solutions for the exercises of the SLIDES "Programming In Haskell" by Graham Hutton: CHAPTER 9 (Interactive Programs)

act :: IO (Char, Char)
act = do x <- getChar
         getChar -- NOTE: added because when pressing ENTER will generate \n which we want to ignore
         y <- getChar
         return (x,y)
         
strlen :: IO ()
strlen  = do putStr "Enter a string: "
             xs <- getLine
             putStr "The string has "
             putStr (show (length xs))
             putStrLn " characters"

------------------------------------------------------------------------------------------------
-- HANGMAN

hangman :: IO ()
hangman = do putStr "First Player please input secret word: "
             secretWord <- getLine
             putStrLn "Second Player start guessing, you can guess 5 times wrong..."
             hmIter (secretWord, 5, replicate (length secretWord) False)

hmIter :: (String,Int,[Bool]) -> IO ()
hmIter (secretWord,errorsLeft,guessPreds) = do guess <- getChar
                                               getChar
                                               let (correctGuess,updatedGuessPreds) = hmCheckGuess guess (secretWord,guessPreds)
                                               if correctGuess then
                                                 hmCorrectGuess (secretWord,errorsLeft,updatedGuessPreds)
                                               else
                                                 hmWrongGuess (secretWord,errorsLeft,updatedGuessPreds)
                                               

hmWrongGuess :: (String,Int,[Bool]) -> IO ()
hmWrongGuess (secretWord,errorsLeft,guessPreds) = if (errorsLeft-1) == 0 then
                                                    putStrLn "You Lost!"
                                                  else
                                                    do putStr "Wrong Guess, you can make "
                                                       putStr $ show (errorsLeft-1)
                                                       putStrLn " mistakes until you loose!"
                                                       hmIter (secretWord,(errorsLeft-1),guessPreds)

hmCorrectGuess :: (String,Int,[Bool]) -> IO ()
hmCorrectGuess (secretWord,errorsLeft,guessPreds) = if hmAllCorrect(guessPreds) then
                                                      putStrLn "You Won!"
                                                    else
                                                    do putStrLn "Correct Guess"
                                                       putStrLn (hmHintWord secretWord guessPreds)
                                                       hmIter (secretWord,errorsLeft,guessPreds)
  
hmCheckGuess :: Char -> (String,[Bool]) -> (Bool,[Bool])
hmCheckGuess guess (secretWord,guessPreds) = (correctGuess,updatedGuessPreds)
  where
    newGuessPred = (hmGuessPred guess secretWord)
    updatedGuessPreds = foldr (\(x,y) acc -> (x || y):acc) [] (zip newGuessPred guessPreds)
    correctGuess = elem True newGuessPred

hmHintWord :: String -> [Bool] -> String
hmHintWord secretWord guessPred = foldr (\(p,c) acc -> if p then c:acc else '*':acc ) [] (zip guessPred secretWord)

hmGuessPred :: (Eq a) => a -> [a] -> [Bool]
hmGuessPred c str = foldr (\x acc -> if x == c then True:acc else False:acc ) [] str

hmAllCorrect :: [Bool] -> Bool
hmAllCorrect newGuessPreds = all (==True) newGuessPreds

indexOf :: (Eq a) => a -> [a] -> [Int]
indexOf c str = foldr (\(x,i) acc -> if x == c then i:acc else acc ) [] (zip str [0..length(str)-1])

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------
{-- EXERCISE: Implement the game of nim in Haskell

The board comprises five rows of stars:
1: * * * * *
2: * * * *
3: * * *
4: * *
5: *

Two players take it turn about to remove one or more stars from the end of a single row.

The winner is the player who removes the last star or stars from the board.

Hint:

Represent the board as a list of five integers that give the number of stars remaining on each row. For example, the initial board is [5,4,3,2,1].
--}

nim :: IO ()
nim = do let initBoard = [5,4,3,2,1]
         nimPlayerMove 1 initBoard

nimPlayerMove :: Int -> [Int] -> IO ()
nimPlayerMove playerIdx board = do nimPrintBoard board
                                   putStrLn ("Player " ++ (show playerIdx) ++ " move")
                                   putStr "Row: "
                                   r <- getLine
                                   putStr "Stars: "
                                   s <- getLine
                                   let row = read r :: Int
                                   let stars = read s :: Int
                                   let newBoard = nimProcessMove (row,stars) board
                                   if (nimCheckWon newBoard) then
                                     putStrLn ("Player " ++ (show playerIdx) ++ " Wins!")
                                   else (nimPlayerMove (nextPlayerIdx playerIdx) newBoard)

-- NOTE: the following code is an exercise to rewrite the function without do-notation (the result terribly ugly and no one can decipher the meaning or extend it anymore)
{-
nimPlayerMove playerIdx board = nimPrintBoard board >> putStrLn ("Player " ++ (show playerIdx) ++ " move") >> putStr "Row: " >> getLine >>= (\r -> putStr "Stars:" >> getLine >>= (\s -> (\stars -> (\row -> (\newBoard -> if (nimCheckWon newBoard) then putStrLn ("Player " ++ (show playerIdx) ++ " Wins!") else (nimPlayerMove (nextPlayerIdx playerIdx) newBoard)) (nimProcessMove (row,stars) board)) (read r :: Int)) (read s :: Int)))
-}

nextPlayerIdx :: Int -> Int
nextPlayerIdx 1 = 2
nextPlayerIdx 2 = 1

-- NOTE: this is usesless since we cannot extract the Int from IO-Monad (one-way)
nimReadInt :: IO Int
nimReadInt = do i <- getLine
                return (read i)

nimPrintBoard :: [Int] -> IO ()
nimPrintBoard board = do putStrLn $ boardToString board

nimProcessMove :: (Int,Int) -> [Int] -> [Int]
nimProcessMove (row,stars) board = foldr (\(x,i) acc -> if i == row then (max 0 (x-stars)):acc else x:acc ) [] (zip board [1..length(board)])

nimCheckWon :: [Int] -> Bool
nimCheckWon board = (sum board) == 0

boardToString :: [Int] -> String
boardToString board = foldr (\(x,i) acc -> "Row " ++ (show i) ++ ": " ++ (replicate x '*') ++ '\n':acc ) [] (zip board [1..length(board)])
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
