module Export

sirAggregateToString : (Nat, Nat, Nat) -> String
sirAggregateToString (susceptibleCount, infectedCount, recoveredCount)
  = show susceptibleCount ++ "," ++ show infectedCount ++ "," ++ show recoveredCount ++ ";"

export
writeMatlabFile : String -> List (Nat, Nat, Nat) -> IO ()
writeMatlabFile fileName dynamics = do
  Right fileHdl <- openFile fileName WriteTruncate
    | Left err => print "Can't open file"
  fPutStrLn fileHdl "dynamics = ["
  traverse_ (fPutStrLn fileHdl . sirAggregateToString) dynamics
  fPutStrLn fileHdl "];"

  fPutStrLn fileHdl "susceptible = dynamics (:, 1);"
  fPutStrLn fileHdl "infected = dynamics (:, 2);"
  fPutStrLn fileHdl "recovered = dynamics (:, 3);"
  fPutStrLn fileHdl "totalPopulation = susceptible(1) + infected(1) + recovered(1);"

  fPutStrLn fileHdl "susceptibleRatio = susceptible ./ totalPopulation;"
  fPutStrLn fileHdl "infectedRatio = infected ./ totalPopulation;"
  fPutStrLn fileHdl "recoveredRatio = recovered ./ totalPopulation;"

  fPutStrLn fileHdl "steps = length (susceptible);"
  fPutStrLn fileHdl "indices = 0 : steps - 1;"

  fPutStrLn fileHdl "figure"
  fPutStrLn fileHdl "plot (indices, susceptibleRatio.', 'color', 'blue', 'linewidth', 2);"
  fPutStrLn fileHdl "hold on"
  fPutStrLn fileHdl "plot (indices, infectedRatio.', 'color', 'red', 'linewidth', 2);"
  fPutStrLn fileHdl "hold on"
  fPutStrLn fileHdl "plot (indices, recoveredRatio.', 'color', 'green', 'linewidth', 2);"

  fPutStrLn fileHdl "set(gca,'YTick',0:0.05:1.0);"
  
  fPutStrLn fileHdl "xlabel ('Time');"
  fPutStrLn fileHdl "ylabel ('Population Ratio');"
  fPutStrLn fileHdl "legend('Susceptible','Infected', 'Recovered');"

  closeFile fileHdl