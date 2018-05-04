module SIREff

import Data.Vect

import Effects
import Effect.Random
import Effect.State
import Effect.StdIO

import Export
import RandomUtils

contactRate : Double
contactRate = 5.0

infectivity : Double
infectivity = 0.05

illnessDuration : Double
illnessDuration = 15.0

data SIRState 
  = Susceptible 
  | Infected 
  | Recovered

Eq SIRState where
  (==) Susceptible Susceptible = True
  (==) Infected Infected = True
  (==) Recovered Recovered = True
  (==) _ _ = False

Show SIRState where
  show Susceptible = "Susceptible"
  show Infected = "Infected"
  show Recovered = "Recovered"
  
-- we want the dimensions in the environment, something
-- only possible with dependent types. Also we parameterise
-- over the type of the elements, basically its a matrix
Disc2dEnv : (w : Nat) -> (h : Nat) -> (e : Type) -> Type
Disc2dEnv w h e = Vect w (Vect h e)

envToCoord : Disc2dEnv w h e -> Vect (w * h) (Nat, Nat, e)
envToCoord env = envToCoordAux Z env
  where
    envToCoordAux : (x : Nat) -> Disc2dEnv w h e -> Vect (w * h) (Nat, Nat, e)
    envToCoordAux _ [] = []
    envToCoordAux x (col :: cs) = (colToCoord 0 col) ++ (envToCoordAux (S x) cs)
      where
        colToCoord : (y : Nat) -> Vect h e -> Vect h (Nat, Nat, e)
        colToCoord _ [] = []
        colToCoord y (elem :: es) = (x, y, elem) :: (colToCoord (S y) es)
    
printLTE : LTE x y -> IO ()
printLTE {x} {y} _ = putStrLn $ "" ++ show x ++ " LTE " ++ show y

printNonLTE : (LTE x y -> Void) -> IO ()
printNonLTE {x} {y} _ = putStrLn $ "" ++ show x ++ " NOT LTE " ++ show y

testLT : IO ()
testLT = do
    let x = 41
    let w = 42
  
    let dprf = testLEAux x w
    case dprf of
      Yes prf   => printLTE prf
      No contra => printNonLTE contra

  where
    testLEAux :  (x : Nat)
               -> (w : Nat)
               -> Dec (LT x w)
    testLEAux x w = isLTE (S x) w

data WithinBounds : (x : Nat) -> (y : Nat) -> (w : Nat) -> (h : Nat) -> Type where
  IsWithin : (x : Nat) -> (y : Nat) -> LT x w -> LT y h -> WithinBounds x y w h

xOutOfBounds : (xNotLTwPrf : LTE (S x) w -> Void) -> WithinBounds x y w h -> Void
xOutOfBounds xNotLTwPrf (IsWithin _ _ prf _) = xNotLTwPrf prf

yOutOfBounds : (yNotLThPrf : LTE (S y) h -> Void) -> WithinBounds x y w h -> Void
yOutOfBounds yNotLThPrf (IsWithin _ _ _ prf) = yNotLThPrf prf

-- this constructs a proof that x < w AND y < h
isWithinBounds :  (x : Nat) 
               -> (y : Nat) 
               -> (w : Nat) 
               -> (h : Nat) 
               -> Dec (WithinBounds x y w h)
isWithinBounds x y w h =
  case isLTE (S x) w of
    Yes xLTwPrf   => case isLTE (S y) h of
                       Yes yLThPrf   => Yes $ IsWithin x y xLTwPrf yLThPrf
                       No yNotLThPrf => No (yOutOfBounds yNotLThPrf)
    No xNotLTwPrf => No (xOutOfBounds xNotLTwPrf)

-- TODO: do we really need x and n? its already in the LT type?
lteToFin :  (x : Nat)
         -> (n : Nat)
         -> LTE x n
         -> Fin (S n)
lteToFin Z n LTEZero = FZ
lteToFin (S k) (S right) (LTESucc y) 
  = FS (lteToFin k right y)

ltToFin :   (x : Nat)
         -> (n : Nat)
         -> LT x n
         -> Fin n
ltToFin Z (S right) (LTESucc y) = FZ
ltToFin (S k) (S right) (LTESucc y)
  = FS (ltToFin k right y)

testLTEToFin : IO ()
testLTEToFin = do
  let x = 10
  let n = 41
  
  let dlte = isLTE x n

  case dlte of
    No contra => print "not LTE!"
    Yes prf   => do
      let fin = lteToFin x n prf
      print $ finToNat fin

testLTToFin : IO ()
testLTToFin = do
  let x = 40
  let n = 41
  
  let dlt = isLTE (S x) n

  case dlt of
    No contra => putStrLn "not LT!"
    Yes prf   => do
      let fin = ltToFin x n prf
      putStrLn $ show $ finToNat fin

setCell :  WithinBounds x y w h
        -> (elem : e)
        -> Disc2dEnv w h e
        -> Disc2dEnv w h e
setCell {w} (IsWithin x y xLTw yLTh) elem env = 
    let colIdx = ltToFin x w xLTw 
    in  updateAt colIdx (updateCol y yLTh) env
  where
    updateCol :  (y : Nat)
              -> LT y h
              -> Vect h e
              -> Vect h e
    updateCol {h} y prf col = 
      let rowIdx = ltToFin y h prf 
      in  updateAt rowIdx (const elem) col

getCell :  WithinBounds x y w h
        -> Disc2dEnv w h e
        -> e
getCell {w} (IsWithin x y xLTw yLTh) env =
    let colIdx = ltToFin x w xLTw 
    in  indexCol y yLTh (index colIdx env)
  where
    indexCol :  (y : Nat)
             -> LT y h
             -> Vect h e
             -> e
    indexCol {h} y prf col = 
      let rowIdx = ltToFin y h prf 
      in  index rowIdx col

vec : Vect 4 Int
vec = [1,2,3,4]

testUpdateVec : IO ()
testUpdateVec = do
  let vec' = updateAt 3 (+1) vec
  print vec'

-- w and h are the dimensions of the environment =>
-- using this we can guarantee that the coordinates
-- are within bounds, given a proof
data SIRAgent : (w : Nat) -> (h : Nat) -> Type where
  SusceptibleAgent : (x : Nat) -> (y : Nat) -> WithinBounds x y w h -> SIRAgent w h
  InfectedAgent    : Double -> (x : Nat) -> (y : Nat) -> WithinBounds x y w h -> SIRAgent w h
  RecoveredAgent   : (x : Nat) -> (y : Nat) -> WithinBounds x y w h -> SIRAgent w h

Show (SIRAgent w h) where
  show (SusceptibleAgent x y prf) = "SusceptibleAgent @(" ++ show x ++ "/" ++ show y ++ ")"
  show (InfectedAgent rt x y prf) = "InfectedAgent @(" ++ show x ++ "/" ++ show y ++ ")"
  show (RecoveredAgent x y prf) = "RecoveredAgent @(" ++ show x ++ "/" ++ show y ++ ")"

mkSusceptible :  (x : Nat)
              -> (y : Nat)
              -> WithinBounds x y w h
              -> SIRAgent w h
mkSusceptible x y prf = SusceptibleAgent x y prf

mkInfected :  (x : Nat)
           -> (y : Nat)
           -> WithinBounds x y w h
           -> Eff (SIRAgent w h) [RND]
mkInfected x y prf = do 
  dur <- randomExp (1 / illnessDuration)
  pure $ InfectedAgent dur x y prf

mkRecovered :  (x : Nat)
            -> (y : Nat)
            -> WithinBounds x y w h
            -> SIRAgent w h
mkRecovered x y prf = RecoveredAgent x y prf

mkSirAgent :  SIRState
           -> (x : Nat)
           -> (y : Nat)
           -> WithinBounds x y w h
           -> Eff (SIRAgent w h) [RND]
mkSirAgent Susceptible x y prf = pure $ mkSusceptible x y prf
mkSirAgent Infected x y prf = mkInfected x y prf
mkSirAgent Recovered x y prf = pure $ mkRecovered x y prf

infected :  Double 
         -> Double
         -> (x : Nat)
         -> (y : Nat)
         -> WithinBounds x y w h
         -> Eff (SIRAgent w h) [STATE (Disc2dEnv w h SIRState), RND]
infected dt recoveryTime x y prf =
  if recoveryTime - dt > 0
    then pure $ InfectedAgent (recoveryTime - dt) x y prf 
    else pure $ mkRecovered x y prf -- TODO: set recovered on environment

susceptible :  (x : Nat)
            -> (y : Nat)
            -> WithinBounds x y w h
            -> Eff (SIRAgent w h) [STATE (Disc2dEnv w h SIRState), RND]
susceptible x y prf = do
    numContacts <- randomExp (1 / contactRate)
    infFlag     <- makeContact (fromIntegerNat $ cast numContacts)
    if infFlag
      then mkInfected x y prf -- TODO: set infected on environment
      else pure $ mkSusceptible x y prf
  where
    makeContact :  Nat 
                -> Eff Bool [STATE (Disc2dEnv w h SIRState), RND] -- TODO: avoid boolean blindness, produce a proof that the agent was infected 
    makeContact Z = pure False
    makeContact (S n) = do
      -- TODO: make contact with other agents through env
      flag <- randomBool infectivity 
      if flag
        then pure True
        else makeContact n

sirAgent :  Double 
         -> SIRAgent w h 
         -> Eff (SIRAgent w h) [STATE (Disc2dEnv w h SIRState), RND]
sirAgent _ (SusceptibleAgent x y prf)  = susceptible x y prf
sirAgent dt (InfectedAgent rt x y prf) = infected dt rt x y prf
sirAgent _ recAg                       = pure recAg

||| this will create an environment with the agents on it
||| with a single infected at the center of the env
-- TODO: will fail when w OR h = 0, can we enforce a w and h > 0 here?
-- TODO: proof that each field is occupied by exactly one agent and that the fields state corresponds to the agents state
--       => we cannot find a pair of agents whose coordinates are the same
-- TODO: use Vect instead of List
createSIR :  (w : Nat) 
          -> (h : Nat)
          -> Eff (Disc2dEnv w h SIRState, List (SIRAgent w h)) [RND] 
createSIR w h = do
    let cx = divNat w 2 
    let cy = divNat h 2 

    let env = Data.Vect.replicate w (Data.Vect.replicate h Susceptible)
    
    let dprf = isWithinBounds cx cy w h
    case dprf of
      No contra => pure (env, []) -- occurs when w or h = 0 => 0 cells, 0 agents
      Yes prf   => do
        let env' = setCell prf Infected env
        let ec = envToCoord env'
        as <- createAgents w h ec
        pure (env', as)
  where
    createAgents :   (w : Nat)
                  -> (h : Nat)
                  -> Vect len (Nat, Nat, SIRState) 
                  -> Eff (List (SIRAgent w h)) [RND]
    createAgents _ _ [] = pure []
    createAgents w h ((x, y, s) :: cs) = do
      let dprf = isWithinBounds x y w h
      case dprf of
        Yes prf   => do
          a <- mkSirAgent s x y prf
          as <- createAgents w h cs
          pure (a :: as)
        No contra => ?bla -- TODO: can we somehow omit this case completely by encoding it in the types statically?

testCreateSIR : IO ()
testCreateSIR = do
  let ret = runPureInit [42] (createSIR 9 9)
  let (e, as) = ret -- no idea why idris doesnt allow this directly

  let ec = envToCoord e

  putStrLn $ show ec

  --putStrLn $ show e
  --putStrLn $ show as

isSus : SIRAgent x y -> Bool
isSus (SusceptibleAgent _ _ _) = True
isSus _ = False

isInf : SIRAgent w h -> Bool
isInf (InfectedAgent _ _ _ _) = True
isInf _ = False

isRec : SIRAgent x y -> Bool
isRec (RecoveredAgent _ _ _) = True
isRec _ = False

-- TODO: use Vect instead of List for agents
runAgents :  Double
          -> List (SIRAgent w h)
          -> Eff (List (Nat, Nat, Nat)) [STATE (Disc2dEnv w h SIRState), RND]
runAgents dt as = runAgentsAcc as []
  where
    runAllAgents :  List (SIRAgent w h)
                 -> List (SIRAgent w h)
                 -> Eff (List (SIRAgent w h)) [STATE (Disc2dEnv w h SIRState), RND]
    runAllAgents [] acc = pure acc 
    runAllAgents (a :: as) acc = do
      a' <- sirAgent dt a
      runAllAgents as (a' :: acc)

    runAgentsAcc :  List (SIRAgent w h)
                 -> List (Nat, Nat, Nat) 
                 -> Eff (List (Nat, Nat, Nat)) [STATE (Disc2dEnv w h SIRState), RND]
    runAgentsAcc as acc = do
      let nSus = length $ filter isSus as
      let nInf = length $ filter isInf as
      let nRec = length $ filter isRec as

      let step = (nSus, nInf, nRec)

      if nInf == 0
        then pure (reverse (step :: acc))
        else do
          let nSum = cast {to=Double} (nSus + nRec + nInf)
          let infFract = cast {to=Double} nInf / nSum

          -- TODO: why is mapE runAgent as not working????
          as' <- runAllAgents as []
          runAgentsAcc as' (step :: acc)
          
runSIR : Eff (List (Nat, Nat, Nat)) [RND]
runSIR = do
  (e, as) <- createSIR 2 2
  -- TODO: instead of runPureInit, add new resource here: STATE
  -- ret <- new e (runAgents 1.0 as)
  let ret = runPureInit [e, 42] (runAgents 1.0 as)
  pure ret

main : IO ()
main = do
  let dyns = runPureInit [42] runSIR
  writeMatlabFile "sirEff.m" dyns