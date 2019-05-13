module Export.CSV where

import System.IO
import Text.Printf

writeCSVFile :: String -> [(Double, (Int, Int, Int))] -> IO ()
writeCSVFile fileName xs = do
  hdl <- openFile fileName WriteMode

  hPutStrLn hdl "T,S,I,R"
  mapM_ (hPutStrLn hdl . sirDynamicToString) xs

  hClose hdl

sirDynamicToString :: (Double, (Int, Int, Int)) -> String
sirDynamicToString (t, (s, i, r)) =
  printf "%.4f" t ++
  "," ++ printf "%d" s ++
  "," ++ printf "%d" i ++ 
  "," ++ printf "%d" r ++
  ";"
