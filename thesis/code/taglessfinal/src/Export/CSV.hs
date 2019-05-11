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
  printf "%.4f" t ++
  "," ++ printf "%.4f" s ++
  "," ++ printf "%.4f" i ++ 
  "," ++ printf "%.4f" r ++
  ";"
