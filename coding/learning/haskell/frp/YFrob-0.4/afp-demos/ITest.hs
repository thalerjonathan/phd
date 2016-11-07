--
-- ITest.hs -- An interactive command-line interpreter for running
-- a series of interactive tests.

module ITest where

import System.IO (hFlush, stdout)
import Data.List (sortBy, isPrefixOf)

-- type synonym for a test-case that is a simple IO action:
type IOTest=(String,IO ())

-- utility function:
fst3 (x,_,_) = x

-- testShell is the top-level command interpreter.
-- arguments:
--   tests -- a list of simple IOTests
--   prompt -- the prompt to display to the user
--   args -- command-line arguments
testShell :: [IOTest] -> String -> [String] -> IO ()
testShell tests prompt args =
  if ((length args)==0)
     then putStrLn "\":help\" for help." >> iShell tests prompt
     else loop args
  where loop [] = return ()
	loop (tname:ts) = do done <- runTest tests tname
			     if done then return () else loop ts

-- interactive shell:
iShell :: [IOTest] -> String -> IO ()
iShell tests prompt =
  do putStr prompt
     hFlush stdout
     line <- getLine
     done <- case words line of
	       ((':':cmd):args) -> handleBuiltin tests cmd args
	       (tname:args) -> runTest tests tname
	       _ -> return False
     if done 
	then return () 
	else iShell tests prompt

-- runSafe:  run a specific IO action, and catch any exceptions:
runSafe :: IO () -> IO ()
runSafe act =
  catch act (\e -> do putStrLn ("*** Exception running test: ")
	              putStrLn (show e)
                      putStrLn "***")

-- run a specific simple IO test
runTest :: [IOTest] -> String -> IO Bool
runTest tests tname =
  let ts = filter ((== tname) . fst) tests
  in do case ts of
          ((nm,tf):[]) -> do putStrLn ("executing test '" ++ nm++ "'")
			     runSafe tf
			     putStrLn ("(test complete)")
	  _ -> do putStrLn ("unknown or ambiguous test '" ++ tname ++ "'")
		  putStrLn ("Use ':list' to list available tests.")
        return False

builtins :: [(String,String,[IOTest] -> [String] -> IO Bool)]
builtins = [("quit","exit the test shell",(\_ _ -> return True))
	    , ("help","display this list of commands", binHelp)
	    , ("list","list available test cases", binList)
	   ]

-- help builtin:
binHelp :: [IOTest] -> [String] -> IO Bool
binHelp tests args =
  do putStrLn ("Command summary:")
     mapM_ putStrLn hlist
     return False
  where hlist = map aux (sortBy ccomp builtins)
	aux (cmd,desc,_) = ("  :" ++ cmd ++ " -- " ++ desc)
	ccomp (cmd1,_,_) (cmd2,_,_) = compare cmd1 cmd2

-- list builtin:
binList :: [IOTest] -> [String] -> IO Bool
binList tests args =
  do putStrLn ("Available Tests:")
     mapM_ putStrLn tnms
     return False
  where tnms = map aux (sortBy tcomp tests)
	aux (tnm,_) = "  " ++ tnm
	tcomp (tnm1,_) (tnm2,_) = compare tnm1 tnm2

-- process a built-in command
handleBuiltin :: [IOTest] -> String -> [String] -> IO Bool
handleBuiltin tstate cmd args = 
  let bins = filter (isPrefixOf cmd . fst3) builtins
  in case bins of
       ((_,_,cmdf):[]) -> cmdf tstate args
       _ -> binError cmd

binError :: String -> IO Bool
binError cmd =
  do putStrLn ("Unknown or ambiguous command ':" ++ cmd ++ "'")
     putStrLn ("Use ':help' for help.")
     return False
	       
     
