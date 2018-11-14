module Main where

import Data.Char
import Data.List
import System.IO

import Options.Applicative

import SugarScape.Export.ExportRunner
import SugarScape.Visual.GlossRunner
import SugarScape.Visual.Renderer
import SugarScape.Core.Model
import SugarScape.Core.Simulation

data Output = Console Int                  -- steps
            | File Int String              -- steps, filename
            | Visual Int AgentVis SiteVis  -- render-freq, agent vis, site-vis

data Params = Params 
  { paramScenario :: String
  , paramOutput   :: Output
  , paramRngSeed  :: Maybe Int
  }

instance Show Output where
  show (Console steps)     = "CONSOLE " ++ show steps ++ 
                              " (print output after " ++ show steps ++ 
                              " steps to console)"
  show (File steps file)   = "FILE " ++ show steps ++ " " ++ show file ++ 
                              " (write output of " ++ show steps ++ " steps to file " ++ show file ++ ")"
  show (Visual freq av sv) = "VISUAL "  ++ show freq ++ " " ++ show av ++ " " ++ show sv ++
                              " (render " ++ show freq ++ 
                              " steps per second, Agent-Visualisation: " ++ show av ++ 
                              ", Site-Visualisation: " ++ show sv ++ ")"

parseParams :: Parser Params
parseParams 
  = Params 
    <$> strOption
      (  long "scenario"
      <> short 's'
      <> metavar "SCENARIO"
      <> help "SugarScape scenario to run e.g. \"Animation II-2\"" )
    <*> parseOutput
    <*> option auto
      (  long "rng"
      <> help "Fixing rng seed"
      <> value Nothing
      <> metavar "INT" )

parseOutput :: Parser Output
parseOutput = fileOut  <|> 
              consoleOut -- <|> 
              -- visualOut

consoleOut :: Parser Output
consoleOut = Console <$> option auto
              (  long "consolesteps"
              <> help "Print output to console after number of steps"
              <> value 1000
              <> metavar "INT" )

fileOut :: Parser Output
fileOut = File 
        <$> option auto
          (  long "filesteps"
          <> help "Write each step to output file"
          <> value 1000
          <> metavar "INT" )
        <*> strOption
          (  long "fileout"
          <> value "export/dynamics.m"
          <> metavar "OUTPUTFILE"
          <> help "Output file" )

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    params <- execParser opts
    runSugarscape params
  where
    opts = info (parseParams <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

findScenario :: String 
             -> [SugarScapeScenario]
             -> Maybe SugarScapeScenario
findScenario name0 
    = find (\s -> strToLower (sgScenarioName s) == name)
  where
    strToLower = map toLower
    name       = strToLower name0

runSugarscape :: Params -> IO ()
runSugarscape params = do
  let scenarioName = paramScenario params
      ms           = findScenario scenarioName sugarScapeScenarios

  case ms of
    Nothing -> putStrLn $ "Couldn't find scenario " ++ show scenarioName ++ ", exit."
    Just scenario -> do
      let output    = paramOutput params
          rngSeed   = paramRngSeed params

      putStrLn "Running Sugarscape with... " 
      putStrLn "--------------------------------------------------"
      print scenario
      putStrLn "--------------------------------------------------"

      putStrLn $ "Output Type: \t\t\t" ++ show output
      putStrLn $ "RNG Seed: \t\t\t" ++ maybe "N/A - using default global random number initialisation" show rngSeed
      putStrLn "--------------------------------------------------"

      (initSimState, initEnv) <- initSimulationOpt rngSeed scenario

      case output of 
        Console steps     -> print $ simulateUntil steps initSimState
        File steps file   -> writeSimulationUntil file steps initSimState
        Visual freq av cv -> runGloss scenario initSimState (0, 0, initEnv, []) freq av cv

      putStrLn "\n--------------------------------------------------\n"