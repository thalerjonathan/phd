module Main where

import Data.Char
import Data.List
import System.IO

import Options.Applicative

import SugarScape.Export.ExportRunner
import SugarScape.Visual.GlossRunner
import SugarScape.Visual.Renderer
import SugarScape.Core.Scenario
import SugarScape.Core.Simulation

data Output = File Int String  -- steps, filename
            | Visual Int AgentColoring SiteColoring  -- render-freq, agent vis, site-vis

instance Show Output where
  show (File steps file)   = "FILE " ++ show steps ++ " " ++ show file ++ 
                              " (write output of " ++ show steps ++ " steps to file " ++ show file ++ ")"
  show (Visual 0 ac sc) = "VISUAL MAX " ++ show ac ++ " " ++ show sc ++
                              " (render as many steps per second possible, Agent-Coloring: " ++ show ac ++ 
                              ", Site-Coloring: " ++ show sc ++ ")"
  show (Visual freq ac sc) = "VISUAL "  ++ show freq ++ " " ++ show ac ++ " " ++ show sc ++
                              " (render " ++ show freq ++ 
                              " steps per second, Agent-Coloring: " ++ show ac ++ 
                              ", Site-Coloring: " ++ show sc ++ ")"

data Options = Options 
  { optScenario :: String
  , optOutput   :: Output
  , optRngSeed  :: Maybe Int
  }

-- RUNNING FROM COMMAND LINE EXAMPLES (using stack)
-- clear & stack exec -- sugarscape-concurrent -s "Figure IV-3" -f 1000 -o export/dynamics.m -r 42
-- clear & stack exec -- sugarscape-concurrent -s "Figure IV-3" -v 0 --ac Default --sc Resource -r 42

-- TODOs
-- BUG: Mating, Trading and Lending blocks.
--   REASON: assuming agent A processes its TickStart message and sends
--   a Mating/Trading/Lending/Request to agent B. If agent B has not processed
--   its TickStart message yet, it could be well the case that this agent B
--   will send a Mating/Trading/Lending Request to agent A (which is quite likely
--   because they are neighbours, otherwise wouldnt happen). This will end up both
--   waiting for the other agents reply on the TMVar channels, resulting in a dead-lock.
------------------------------------------------------------------------------------
---  SOLUTION: using orElse: in case the TMVar blocks, read from the queue,
--     if this blocks then try to read the TMVar again until either one responds
--     then we either can have progress on the TMVar or on the Queue:
--     in case of the TMVar all is good and we continue. In case of the queue:
--     check if the message is of type DomainEventWithReply, in this case, 
--     we know that both block on each other. In other cases just put the message back in the queue.
--     Problem: we cannot just start processing that message because the agent is internally in a different state.
--     Also we can't go back to previous agent MSF because might have had 
--     changes in the environment or other changes in STM which cannot be
--     rolled back. NO IDEA SO FAR HOW TO SOLVE THIS!
--
-- FUNDAMENTAL PROBLEM OF CONCURRENT APPROACH: due to asynchronous events it is
--   might well be the case that the environment changes in between the initial
--   agent message and its continuation: e.g. when sending a TradingOffer to 
--   another agent, that other agents MRS might have changed in the mean time
--   already
-- 
-- CONCLUSION: the concurrent approach using STM shows promising results but
--   it seems that it is just not feasible in sugarscape because it implies / requires
--   too sequential processing semantics, especially in the cases of Mating, Trading and Lending.
--   All the other use-cases work perfectly using asynchronous messaging because
--   there is no need for a synchronous reply.
--   => WE LEAVE THE CONCURRENT IMPLEMENTATION AS IT IS FOR NOW AND LEAVE IT AS
--   OPEN RESEARCH QUESTION.
-- ------------------------------------------------------------------------------------  
--  OTHER APPROACH TO SYNC INTERACTIONS
--    Abandon SYNC INTERACTIONS alltogether:
--    Instead of sending e.g. a Mating/Trading/LendingOffer the agent sends 
--    just a Mating/Trading/Lending ENGAGE message without any of its information
--    e.g. MRS,... which is local to the interaction and depends on the current
--    state of the agent. This information will then be exchanged in further asynchronous
--    interactions, which require more steps but this solves the problem of invalid
--    data sent initially. Also this does not require the agent to change
--    the event handler internally. 
--    
--    The reason why we wanted to employ synchronous interactions is because
--    it solves the problem of resource-constraints: if an agent sends a mating
--    request it will spend half of its wealth on its newborn - if the mating
--    request goes through. If we have asynchronous messages these can be interleaved
--    e.g. while waiting for the reply of the mating partner the agent could
--    be engaged in a trade which transacts, suddenly making this agent unable
--    to bear children because its not wealthy enough anymore. 2 ways:
--    1. ignore these situations and assume they will rarely occur: 
--       not very good because they will occur more often than we think, also we want 
--       to replicate the dynamics which will probably not be possible anymore.
--       Also in trading this makes it simply wrong because agents engage in loads of trades
--       and it would create new wealth out of nothing, violating ressource-constraints.
--    2. The agent 'locks' the consumable ressources away until the interaction finishes:
--       this leads to the agent being able to transact other messages but it has
--       virtually less wealth until the interaction really transact. If the 
--       interaction does not transact, the locked ressources will be restored
--       => probably most reasonable approach:
--          - no internal state-changes of message-handlers necessary
--          - no deadlocks possible even in case two agents send each other same offering message
--          - should result in same dynamics 
--          - QUESTION: the selection of neighbours and their state always local to the initiation
--          of the messages: the neighbours and their state could change during the transaction
--          but that will not bother us, we kind of assume a snap-shot at the moment of the
--          Engage message.

-- BUG: loans not working correctly yet when inheritance is turned on
-- CLEANUP: remove unsafePerformIO for reading Environment in Renderer and Tests
-- PERFORMANCE: more than 50% of time used in main thread waiting for all queues to empty.
--  still faster than sequential but can we do better? huge Potential for Performance improvemrnt
-- TESTING: can we add some tests which check for memory-leaks? e.g. running
--  various scenarios and check if memory-consumption is 'normal'? Can we use
--  criterion for that?

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    o <- execParser opts
    runSugarscape o
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Full implementation of the famous SugarScape model by J. Epstein and R. Axtell.")

runSugarscape :: Options -> IO ()
runSugarscape opts = do
  let scenarioName = optScenario opts
      ms           = findScenario scenarioName sugarScapeScenarios

  case ms of
    Nothing -> putStrLn $ "Couldn't find scenario " ++ show scenarioName ++ ", exit."
    Just scenario -> do
      let output    = optOutput opts
          rngSeed   = optRngSeed opts

      (initSimState, initOut, scenario') <- initSimulationOpt rngSeed scenario

      putStrLn "Running Sugarscape with... " 
      putStrLn "--------------------------------------------------"
      print scenario'
      putStrLn "--------------------------------------------------"

      putStrLn $ "RNG Seed: \t\t\t" ++ maybe "N/A - using default global random number initialisation" show rngSeed
      putStrLn $ "Output Type: \t\t\t" ++ show output
      putStrLn "--------------------------------------------------"

      case output of 
        File steps file   -> writeSimulationUntil file steps initSimState
        Visual freq av cv -> runGloss scenario' initSimState initOut freq av cv

      putStrLn "\n--------------------------------------------------\n"

findScenario :: String 
             -> [SugarScapeScenario]
             -> Maybe SugarScapeScenario
findScenario name0 
    = find (\s -> strToLower (sgScenarioName s) == name)
  where
    strToLower = map toLower
    name       = strToLower name0

parseOptions :: Parser Options
parseOptions 
  = Options 
    <$> strOption
      (  long "scenario"
      <> short 's'
      <> metavar "String"
      <> help "SugarScape scenario to run e.g. \"Animation II-2\"")
    <*> parseOutput
    <*> optional (option auto  
      ( long "rng" 
      <> short 'r'
      <> help "Fixing rng seed" 
      <> metavar "Int"))

parseOutput :: Parser Output
parseOutput = fileOut    <|> 
              visualOut

fileOut :: Parser Output
fileOut = File 
        <$> option auto
          (  long "fileout"
          <> short 'f'
          <> help "Write each step to output file"
          <> value 1000
          <> metavar "Int" )
        <*> strOption
          (  long "output"
          <> short 'o'
          <> value "export/dynamics.m"
          <> metavar "String"
          <> help "Output file")
      
visualOut :: Parser Output
visualOut = Visual 
         <$> option auto
           (  long "visual"
           <> short 'v'
           <> help "Visual steps calculated per second without upper limit of steps calculated in total (infinitely running)"
           <> value 0
           <> metavar "Int" )
        <*> option auto
           (  long "ac"
           <> help "Coloring of agents"
           <> value Default
           <> metavar "Default | Gender | Culture | Tribe | Welfare | Disease")
        <*> option auto
           (  long "sc"
           <> help "Coloring of sites"
           <> value Resource
           <> metavar "Resource | Polution")