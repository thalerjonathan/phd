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
  show ecovered = "Recovered"
  
-- we want the dimensions in the environment, something
-- only possible with dependent types. Also we parameterise
-- over the type of the elements, basically its a matrix
Disc2dEnv : (w : Nat) -> (h : Nat) -> (e : Type) -> Type
Disc2dEnv w h e = Vect (S w) (Vect (S h) e)

Disc2dEnvVect : (w : Nat) -> (h : Nat) -> (e : Type) -> Type
Disc2dEnvVect w h e = Vect (S w * S h) e

--envToCoord : Disc2dEnv w h e -> Disc2dEnvVect w h (Nat, Nat, e)
--envToCoord env = envToCoordAux Z env

colToCoord : (x : Nat) -> (y : Nat) -> Vect (S colSize) e -> Vect (S colSize) (Nat, Nat, e)
colToCoord x y (elem :: es) = 
  let c = (x, y, elem) 
  --    ret = (colToCoord (S y) es)
  in  ?colToCoord_rhs -- ((x, y, elem) :: ret)

mutual
  colToCoordAux :  (x : Nat) 
                -> (col : Vect (S h) e) 
                -> (cs : Vect w (Vect (S h) e)) 
                -> Vect (S (plus h (mult w (S h)))) (Nat, Nat, e)
  colToCoordAux x col cs = (colToCoord x Z col) ++ (envToCoordAux (S x) cs)

  envToCoordAux : (x : Nat) ->  Disc2dEnv w h e -> Disc2dEnvVect w h (Nat, Nat, e)
  envToCoordAux x (col :: cs) = colToCoordAux x col cs -- (colToCoord Z col) ++ (envToCoordAux (S x) cs)
  {-}
  where
    colToCoord : (y : Nat) -> Vect (S colSize) e -> Vect (S colSize) (Nat, Nat, e)
    colToCoord y (elem :: es) = 
      let c = (x, y, elem) 
      --    ret = (colToCoord (S y) es)
      in  ?colToCoord_rhs -- ((x, y, elem) :: ret)

        --colToCoord _ [] = []
        --colToCoord y (elem :: es) = (x, y, elem) :: (colToCoord (S y) es)
    

    envToCoordAux _ [] = []
    envToCoordAux x (col :: cs) = (colToCoord 0 col) ++ (envToCoordAux (S x) cs)
      where
        colToCoord : (y : Nat) -> Vect h e -> Vect h (Nat, Nat, e)
        colToCoord _ [] = []
        colToCoord y (elem :: es) = (x, y, elem) :: (colToCoord (S y) es)
        -}

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

-- TODO: implement, it should be possible
-- to generate a random bound
-- but only in case w and h > 0
randomBounds :  Disc2dEnv w h e
             -> Eff (WithinBounds x y w h) [RND]

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

data MakeContact : (w : Nat) -> (h : Nat) -> Type where
  ContactWith : SIRState -> WithinBounds x y w h -> MakeContact w h

makeRandomContact :  Disc2dEnv w h SIRState
                  -> Eff (MakeContact w h) [RND]
makeRandomContact env = do
  prfRnd <- randomBounds env
  let c = getCell prfRnd env
  pure $ ContactWith c prfRnd

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
    else do
      -- TODO: can we specify that we must set the cell to Recovered?
      update (setCell prf Recovered)
      -- TODO: can only go to recovered or stay Infected, can we encode this in types?
      pure $ mkRecovered x y prf

susceptible :  (x : Nat)
            -> (y : Nat)
            -> WithinBounds x y w h
            -> Eff (SIRAgent w h) [STATE (Disc2dEnv w h SIRState), RND]
susceptible x y prf = do
    numContacts <- randomExp (1 / contactRate)
    infFlag     <- makeContact (fromIntegerNat $ cast numContacts)
    if infFlag
      then do
        -- TODO: can we specify that we must set the cell to Infected when becoming infected?
        update (setCell prf Infected)
        -- TODO: can only go to Infected or stay Susceptible, can we encode this in types?
        mkInfected x y prf 
      else pure $ mkSusceptible x y prf
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
sirAgent _ (SusceptibleAgent x y prf)  = susceptible x y prf
sirAgent dt (InfectedAgent rt x y prf) = infected dt rt x y prf
sirAgent _ recAg                       = pure recAg

||| this will create an environment with the agents on it
||| with a single infected at the center of the env
-- TODO: will fail when w OR h = 0, can we enforce a w and h > 0 here?
-- TODO: proof that each field is occupied by exactly one agent and that the fields state corresponds to the agents state
--       => we cannot find a pair of agents whose coordinates are the same
createSIR :  (w : Nat) 
          -> (h : Nat)
          -> Eff (Disc2dEnv w h SIRState, Vect (w * h) (SIRAgent w h)) [RND] 
createSIR w h = do
    let cx = divNat w 2 
    let cy = divNat h 2 

    let env = Data.Vect.replicate w (Data.Vect.replicate h Susceptible)
    
    let dprf = isWithinBounds cx cy w h
    case dprf of
      -- this case should only occur when w and h = 0, this can't 
      -- be inferred by the compiler, thus we need to satisfy it,
      -- which means we have to construct the agents instead of 
      -- just returning an [] empty vector
      No contra => do 
        let ec = envToCoord env
        as <- createAgents w h ec
        pure (env, as)
      Yes prf   => do
        let env' = setCell prf Infected env
        let ec = envToCoord env'
        as <- createAgents w h ec
        pure (env', as)
  where
    createAgents :   (w : Nat)
                  -> (h : Nat)
                  -> Vect len (Nat, Nat, SIRState) 
                  -> Eff (Vect len (SIRAgent w h)) [RND]
    createAgents _ _ [] = pure []
    createAgents w h ((x, y, s) :: cs) = do
      let dprf = isWithinBounds x y w h
      case dprf of
        Yes prf   => do
          a <- mkSirAgent s x y prf
          as <- createAgents w h cs
          pure (a :: as)
        No contra => ?willnotoccur -- TODO: can we somehow omit this case completely by encoding it in the types statically?

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
          let nSum = cast {to=Double} (nSus + nRec + nInf)
          let infFract = cast {to=Double} nInf / nSum

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

  -}