module Export.CSV where

import System.IO
import Text.Printf

writeCSVFile :: String -> [(Double, (Double, Double, Double))] -> IO ()
writeCSVFile fileName xs = do
  hdl <- openFile fileName WriteMode

  hPutStrLn hdl "T,S,I,R"
  mapM_ (hPutStrLn hdl . sirDynamicToString) xs

  hClose hdl

sirDynamicToString :: (Double, (Double, Double, Double)) -> String
sirDynamicToString (t, (s, i, r)) =
  printf "%.2f" t ++
  "," ++ printf "%.2f" s ++
  "," ++ printf "%.2f" i ++ 
  "," ++ printf "%.2f" r
