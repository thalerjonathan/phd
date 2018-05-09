module SIREff

import Data.Vect
import Effects
import Effect.Random
import Effect.State
import Effect.StdIO

import Disc2dEnv
import Export
import RandomUtils
import SIRState

%default total

contactRate : Double
contactRate = 5.0

infectivity : Double
infectivity = 0.05

illnessDuration : Double
illnessDuration = 15.0

-- because we have now (S w) and (S h) in the envirnoment
-- we can immediately use x : Fin w and y : Fin h which 
-- guarantees strictly LT, thus we shouldnt need any LT proofs anymore

-- w and h are the dimensions of the environment =>
-- using this we can guarantee that the coordinates
-- are within bounds, given a proof
data SIRAgent : (w : Nat) -> (h : Nat) -> Type where
  SusceptibleAgent : Disc2dCoords w h -> SIRAgent w h
  InfectedAgent    : Double -> Disc2dCoords w h -> SIRAgent w h
  RecoveredAgent   : Disc2dCoords w h -> SIRAgent w h

Show (SIRAgent w h) where
  show (SusceptibleAgent c) = "SusceptibleAgent @" ++ show c
  show (InfectedAgent rt c) = "InfectedAgent @" ++ show c
  show (RecoveredAgent c) = "RecoveredAgent @" ++ show c

data MakeContact : (w : Nat) -> (h : Nat) -> Type where
  ContactWith : SIRState -> Disc2dCoords w h -> MakeContact w h

makeRandomGlobalContact :  Disc2dEnv w h SIRState
                        -> Eff (MakeContact w h) [RND]
makeRandomGlobalContact {w} {h} env = do
  x <- assert_total $ rndFin w -- TODO: why is rndFin not total???
  y <- assert_total $ rndFin h -- TODO: why is rndFin not total???
  let c = mkDisc2dCoords x y
  let s = getCell c env
  pure $ ContactWith s c

makeRandomNeighbourContact :  Disc2dCoords w h
                           -> Disc2dEnv w h SIRState
                           -> Eff (Maybe (MakeContact w h)) [RND]
makeRandomNeighbourContact ref env = do 
  let (n ** ns) = filterNeighbourhood ref neumann env
  ri <- assert_total $ rndInt 0 (cast n) -- TODO: why is rndFin not total???
  let mi = integerToFin ri n
  
  case mi of 
    Nothing  => pure Nothing
    Just idx => do
      let (coord, s) = index idx ns
      pure $ Just $ ContactWith s coord

mkSusceptible :  Disc2dCoords w h
              -> SIRAgent w h
mkSusceptible c = SusceptibleAgent c

mkInfected :  Disc2dCoords w h
           -> Eff (SIRAgent w h) [RND]
mkInfected c = do 
  dur <- randomExp (1 / illnessDuration)
  pure $ InfectedAgent dur c

mkRecovered :  Disc2dCoords w h
            -> SIRAgent w h
mkRecovered c = RecoveredAgent c

mkSirAgent :  SIRState
           -> Disc2dCoords w h
           -> Eff (SIRAgent w h) [RND]
mkSirAgent Susceptible c = pure $ mkSusceptible c
mkSirAgent Infected c = mkInfected c
mkSirAgent Recovered c = pure $ mkRecovered c

infected :  Double 
         -> Double
         -> Disc2dCoords w h
         -> Eff (SIRAgent w h) [STATE (Disc2dEnv w h SIRState)] -- , RND]
infected dt recoveryTime c@(MkDisc2dCoords x y) =
  if recoveryTime - dt > 0
    then pure $ InfectedAgent (recoveryTime - dt) c
    else do
      -- TODO: can we specify that we must set the cell to Recovered?
      update (setCell c Recovered)
      -- TODO: can only go to recovered or stay Infected, can we encode this in types?
      pure $ mkRecovered c

susceptible :  Disc2dCoords w h
            -> Eff (SIRAgent w h) [STATE (Disc2dEnv w h SIRState), RND]
susceptible coord = do
    numContacts <- randomExp (1 / contactRate)
    --infFlag     <- contactsWithNeighbourInfected (fromIntegerNat $ cast numContacts) coord
    infFlag     <- contactsWithGlobalInfected (fromIntegerNat $ cast numContacts)
    if infFlag
      then do
        -- TODO: can we specify that we must set the cell to Infected when becoming infected?
        update (setCell coord Infected)
        -- TODO: can only go to Infected or stay Susceptible, can we encode this in types?
        mkInfected coord
      else pure $ mkSusceptible coord
  where
    contactsWithGlobalInfected :  Nat 
                               -> Eff Bool [STATE (Disc2dEnv w h SIRState), RND] 
    contactsWithGlobalInfected Z = pure False
    contactsWithGlobalInfected (S n) = do
      env <- get
      
      (ContactWith s _) <- makeRandomGlobalContact env
      
      if Infected == s
        then do
          flag <- randomBool infectivity 
          if flag
            then pure True
            else contactsWithGlobalInfected n
        else contactsWithGlobalInfected n
        
    contactsWithNeighbourInfected :  Nat 
                                  -> Disc2dCoords w h
                                  -> Eff Bool [STATE (Disc2dEnv w h SIRState), RND] 
    contactsWithNeighbourInfected Z _ = pure False
    contactsWithNeighbourInfected (S n) coord = do
      env <- get

      mayContact <- makeRandomNeighbourContact coord env
      case mayContact of
        Nothing => contactsWithNeighbourInfected n coord
        Just (ContactWith s _) => 
          if Infected == s
            then do
              flag <- randomBool infectivity 
              if flag
                then pure True
                else contactsWithNeighbourInfected n coord
            else contactsWithNeighbourInfected n coord

sirAgent :  Double 
         -> SIRAgent w h 
         -> Eff (SIRAgent w h) [STATE (Disc2dEnv w h SIRState), RND]
sirAgent _ (SusceptibleAgent c)  = susceptible c
sirAgent dt (InfectedAgent rt c) = infected dt rt c
sirAgent _ recAg                 = pure recAg

||| this will create an environment with the agents on it
||| with a single infected at the center of the env
-- TODO: proof that each field is occupied by exactly one agent and that the fields state corresponds to the agents state
--       => we cannot find a pair of agents whose coordinates are the same
createSIR :  (w : Nat) 
          -> (h : Nat)
          -> Eff (Disc2dEnv w h SIRState, Vect (S w * S h) (SIRAgent w h)) [RND] 
createSIR w h = do
    let env0 = initDisc2dEnv w h Susceptible
    let c    = centreCoords env0
    let env  = setCell c Infected env0
    let ec   = envToCoord env

    as <- createAgents w h ec

    pure (env, as)
  where
    createAgents :   (w : Nat)
                  -> (h : Nat)
                  -> Vect len (Nat, Nat, SIRState) 
                  -> Eff (Vect len (SIRAgent w h)) [RND]
    createAgents _ _ [] = pure []
    createAgents w h ((x, y, s) :: cs) = do
      -- TODO: this is unsatisfactory, we should get Fin w / Fin h
      -- already instead of Nat, Nat
      let xf = fromMaybe FZ (natToFin x (S w))
      let yf = fromMaybe FZ (natToFin y (S h))
      let c = mkDisc2dCoords xf yf

      a <- mkSirAgent s c
      as <- createAgents w h cs
      pure (a :: as)
  {-
    createAgents :   (w : Nat)
                  -> (h : Nat)
                  -> Vect len (Disc2dCoords w h, SIRState) 
                  -> Eff (Vect len (SIRAgent w h)) [RND]
    createAgents _ _ [] = pure []
    createAgents w h ((c, s) :: cs) = do
      a <- mkSirAgent s c
      as <- createAgents w h cs
      pure (a :: as)
      -}

testCreateSIR : IO ()
testCreateSIR = do
  let ret = runPureInit [42] (createSIR 9 9)
  let (e, as) = ret -- no idea why idris doesnt allow this directly

  let ec = envToCoord e

  putStrLn $ show ec

  --putStrLn $ show e
  --putStrLn $ show as

isSus : SIRAgent w h -> Bool
isSus (SusceptibleAgent _) = True
isSus _ = False

isInf : SIRAgent w h -> Bool
isInf (InfectedAgent _ _) = True
isInf _ = False

isRec : SIRAgent w h -> Bool
isRec (RecoveredAgent _) = True
isRec _ = False

partial
runAgents :  Double
          -> Vect len (SIRAgent w h)
          -> Eff (List (Nat, Nat, Nat)) [STATE (Disc2dEnv w h SIRState), RND]
runAgents dt as = runAgentsAcc as []
  where
    runAllAgents :  Vect len (SIRAgent w h)
                 -> Eff (Vect len (SIRAgent w h)) [STATE (Disc2dEnv w h SIRState), RND]
    runAllAgents [] = pure []
    runAllAgents (a :: as) = do
      a' <- sirAgent dt a
      as' <- runAllAgents as
      pure (a' :: as')

    partial
    runAgentsAcc :  Vect len (SIRAgent w h)
                 -> List (Nat, Nat, Nat) 
                 -> Eff (List (Nat, Nat, Nat)) [STATE (Disc2dEnv w h SIRState), RND]
    runAgentsAcc as acc = do
      let (nSus ** _) = filter isSus as
      let (nInf ** _) = filter isInf as
      let (nRec ** _) = filter isRec as

      let step = (nSus, nInf, nRec)

      if nInf == 0
        then pure (reverse (step :: acc))
        else do
          --let nSum = cast {to=Double} (nSus + nRec + nInf)
          --let infFract = cast {to=Double} nInf / nSum

          -- TODO: why is mapE runAgent as not working????
          as' <- runAllAgents as
          runAgentsAcc as' (step :: acc)

partial
runSIR : Eff (List (Nat, Nat, Nat)) [RND]
runSIR = do
  (e, as) <- createSIR 50 50
  -- TODO: instead of runPureInit, add new resource here: STATE
  -- ret <- new e (runAgents 1.0 as)
  let ret = runPureInit [e, 42] (runAgents 1.0 as)
  pure ret

partial
main : IO ()
main = do
  let dyns = runPureInit [42] runSIR
  writeMatlabFile "sirEff.m" dyns