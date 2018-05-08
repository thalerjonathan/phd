module Disc2dEnv

import Data.Vect

%default total
%access public export

-- we want the dimensions in the environment, something
-- only possible with dependent types. Also we parameterise
-- over the type of the elements, basically its a matrix
-- guaranteed that the environment is always non-empty
Disc2dEnv : (w : Nat) -> (h : Nat) -> (e : Type) -> Type
Disc2dEnv w h e = Vect (S w) (Vect (S h) e)

Disc2dEnvVect : (w : Nat) -> (h : Nat) -> (e : Type) -> Type
Disc2dEnvVect w h e = Vect (S w * S h) e

data Disc2dCoords : (w : Nat) -> (h : Nat) -> Type where
  MkDisc2dCoords : Fin (S w) -> Fin (S h) -> Disc2dCoords w h

Show (Disc2dCoords w h) where
  show (MkDisc2dCoords x y) = "(" ++ (show $ finToNat x) ++ "/" ++ (show $ finToNat y) ++ ")"

initDisc2dEnv : (w : Nat) -> (h : Nat) -> e -> Disc2dEnv w h e
initDisc2dEnv w h e = Data.Vect.replicate (S w) (Data.Vect.replicate (S h) e)

mkDisc2dCoords : Fin (S w) -> Fin (S h) -> Disc2dCoords w h
mkDisc2dCoords x y = MkDisc2dCoords x y

centreCoords : Disc2dEnv w h e -> Disc2dCoords w h
centreCoords {w} {h} _ =
    let x = halfNatToFin w
        y = halfNatToFin h
    in  mkDisc2dCoords x y
  where
    halfNatToFin : (x : Nat) -> Fin (S x)
    halfNatToFin x = 
      let xh   = divNatNZ x 2 SIsNotZ 
          mfin = natToFin xh (S x)
      in  fromMaybe FZ mfin

-- TODO: remove when finished
testPosVect_rhs : (x : Nat) -> (xs : Vect n Nat) -> Vect (S n) Nat
testPosVect_rhs x [] = [x]
testPosVect_rhs x (y :: xs) = (x * x) :: (testPosVect_rhs y xs)
-- TODO: remove when finished
testPosVect : Vect (S n) Nat -> Vect (S n) Nat
testPosVect (x :: xs) = testPosVect_rhs x xs

setCell :  Disc2dCoords w h
        -> (elem : e)
        -> Disc2dEnv w h e
        -> Disc2dEnv w h e
setCell (MkDisc2dCoords colIdx rowIdx) elem env 
    = updateAt colIdx (\col => updateAt rowIdx (const elem) col) env
 
getCell :  Disc2dCoords w h
        -> Disc2dEnv w h e
        -> e
getCell (MkDisc2dCoords colIdx rowIdx) env
    = index rowIdx (index colIdx env)

neumann : Vect 4 (Integer, Integer)
neumann = [         (0,  1), 
           (-1,  0),         (1,  0),
                    (0, -1)]

moore : Vect 8 (Integer, Integer)
moore = [(-1,  1), (0,  1), (1,  1),
         (-1,  0),          (1,  0),
         (-1, -1), (0, -1), (1, -1)]

-- TODO: can we express that n <= len?
filterNeighbourhood :  Disc2dCoords w h
                    -> Vect len (Integer, Integer)
                    -> Disc2dEnv w h e 
                    -> (n ** Vect n (Disc2dCoords w h, e))
filterNeighbourhood {w} {h} (MkDisc2dCoords x y) ns env =
    let xi = finToInteger x
        yi = finToInteger y
    in  filterNeighbourhood' xi yi ns env
  where
    filterNeighbourhood' :  (xi : Integer)
                         -> (yi : Integer)
                         -> Vect len (Integer, Integer)
                         -> Disc2dEnv w h e 
                         -> (n ** Vect n (Disc2dCoords w h, e))
    filterNeighbourhood' _ _ [] env = (0 ** [])
    filterNeighbourhood' xi yi ((xDelta, yDelta) :: cs) env 
      = let xd = xi - xDelta
            yd = yi - yDelta
            mx = integerToFin xd (S w)
            my = integerToFin yd (S h)
        in case mx of
            Nothing => filterNeighbourhood' xi yi cs env 
            Just x  => (case my of 
                        Nothing => filterNeighbourhood' xi yi cs env 
                        Just y  => let coord      = MkDisc2dCoords x y
                                       c          = getCell coord env
                                       (_ ** ret) = filterNeighbourhood' xi yi cs env
                                   in  (_ ** ((coord, c) :: ret)))

envToCoord : Disc2dEnv w h e -> Disc2dEnvVect w h (Nat, Nat, e)
envToCoord (col0 :: cs0) = envToCoordAux Z col0 cs0
  where
    colToCoord : (x : Nat) -> Vect (S colSize) e -> Vect (S colSize) (Nat, Nat, e)
    colToCoord x (elem0 :: es0) = colToCoordAux Z elem0 es0
      where
        colToCoordAux :  (y : Nat) 
                      -> (elem : e) 
                      -> (es : Vect colSize e) 
                      -> Vect (S colSize) (Nat, Nat, e)
        colToCoordAux y elem [] = [(x, y, elem)]
        colToCoordAux y elem (elem' :: es) = (x, y, elem) :: (colToCoordAux (S y) elem' es)
        
    proofLastCol :  (col : Vect (S h) (Nat, Nat, e)) 
                 -> Vect (S (plus h 0)) (Nat, Nat, e)
    proofLastCol {h} col = rewrite plusZeroRightNeutral h in col

    envToCoordAux :  (x : Nat) 
                  -> (col : Vect (S h) e) 
                  -> (cs : Vect w (Vect (S h) e)) 
                  -> Vect (S (plus h (mult w (S h)))) (Nat, Nat, e)
    envToCoordAux {h} x col [] = 
      let col' = colToCoord x col
      in  proofLastCol col' -- need to prove that h + 0 = h (h = plus h 0)
    envToCoordAux x col (colNext :: cs) =
      let col' = colToCoord x col
          ret  = envToCoordAux (S x) colNext cs
      in  col' ++ ret

{-
-- TODO: make this work, but its extremely complex
envToCoord : Disc2dEnv w h e -> Disc2dEnvVect w h (Disc2dCoords w h, e)
envToCoord (col0 :: cs0) = envToCoordAux Z col0 cs0
  where
    colToCoord : (x : Nat) -> Vect (S colSize) e -> Vect (S colSize) (Disc2dCoords w h, e)
    colToCoord x (elem0 :: es0) = colToCoordAux Z elem0 es0
      where
        -- TODO: should use Fin (S h) instead of y
        colToCoordAux :  (y : Nat) 
                      -> (elem : e) 
                      -> (es : Vect colSize e) 
                      -> Vect (S colSize) (Disc2dCoords w h, e)
        colToCoordAux y elem [] = 
          let xf = fromMaybe FZ (natToFin x (S w))
              yf = fromMaybe FZ (natToFin y (S h))
              c = (mkDisc2dCoords xf yf, elem)
          in  [c]
        colToCoordAux y elem (elemNext :: es) = 
          let xf = fromMaybe FZ (natToFin x (S w))
              yf = fromMaybe FZ (natToFin y (S h))
              c = (mkDisc2dCoords xf yf, elem)
          in  c :: (colToCoordAux (S y) elemNext es)
        
    proofLastCol :  (col : Vect (S h) (Disc2dCoords w h, e)) 
                 -> Vect (S (plus h 0)) (Disc2dCoords w h, e)
    proofLastCol {h} col = rewrite plusZeroRightNeutral h in col

    envToCoordAux_rhs :  (col' : Vect (S h1) (Disc2dCoords w1 h2, e)) 
                      -> (ret : Vect (S (plus h1 (mult len (S h1)))) (Disc2dCoords len h2, e)) 
                 -> Vect (S (plus h1 (S (plus h1 (mult len (S h1)))))) (Disc2dCoords (S len) h2, e)
    envToCoordAux_rhs col' ret = 
      let ret' = col' ++ ret
      in  ?envToCoordAux_rhs -- col' ++ ret

    envToCoordAux :  (x : Nat) 
                  -> (col : Vect (S h) e) 
                  -> (cs : Vect w (Vect (S h) e)) 
                  -> Vect (S (plus h (mult w (S h)))) (Disc2dCoords w h, e)
    envToCoordAux x col [] = 
      let col' = colToCoord x col
      in  proofLastCol col' -- need to prove that h + 0 = h (h = plus h 0)
    envToCoordAux x col (colNext :: cols) =
      let col' = colToCoord x col
          ret  = envToCoordAux (S x) colNext cols
      in  envToCoordAux_rhs col' ret -- col' ++ ret
-}