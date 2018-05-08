module SIREff

import Data.Vect

import Effects
import Effect.Random
import Effect.State
import Effect.StdIO

import Disc2dEnv
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
  show ecovered = "Recovered"


{-
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
-}

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

makeRandomContact :  Disc2dEnv w h SIRState
                  -> Eff (MakeContact w h) [RND]
makeRandomContact {w} {h} env = do
  x <- rndFin w
  y <- rndFin h
  let c = mkDisc2dCoords x y
  let s = getCell c env
  pure $ ContactWith s c

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
susceptible c@(MkDisc2dCoords x y) = do
    numContacts <- randomExp (1 / contactRate)
    infFlag     <- makeContact (fromIntegerNat $ cast numContacts)
    if infFlag
      then do
        -- TODO: can we specify that we must set the cell to Infected when becoming infected?
        update (setCell c Infected)
        -- TODO: can only go to Infected or stay Susceptible, can we encode this in types?
        mkInfected c
      else pure $ mkSusceptible c
  where
    makeContact :  Nat 
                -> Eff Bool [STATE (Disc2dEnv w h SIRState), RND] 
    makeContact Z = pure False
    makeContact (S n) = do
      env <- get
      
      (ContactWith s _) <- makeRandomContact env
      if Infected == s
        then do
          flag <- randomBool infectivity 
          if flag
            then pure True
            else makeContact n
        else makeContact n

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

runSIR : Eff (List (Nat, Nat, Nat)) [RND]
runSIR = do
  (e, as) <- createSIR 21 21
  -- TODO: instead of runPureInit, add new resource here: STATE
  -- ret <- new e (runAgents 1.0 as)
  let ret = runPureInit [e, 42] (runAgents 1.0 as)
  pure ret

main : IO ()
main = do
  let dyns = runPureInit [42] runSIR
  writeMatlabFile "sirEff.m" dyns