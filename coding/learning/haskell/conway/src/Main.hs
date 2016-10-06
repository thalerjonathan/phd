module Main where
import System.Random
import System.IO.Unsafe
import Data.Ratio
import Data.Time.Clock.POSIX

xCells :: Int
xCells = 10

yCells :: Int
yCells = 10

cellProb :: Float
cellProb = 0.5

neighbourhood :: [(Int, Int)]
neighbourhood = [(-1,-1), (0, -1), (1, -1), (-1,0), (1, 0), (-1, 1), (0, 1), (1, 1)]

main :: IO ()
main = do field <- initField xCells yCells cellProb
          resultField <- performSimSteps field 10
          return ()


initField :: Int -> Int -> Float -> IO [Int]
initField x y p = randFloats (x * y) >>= (\fs -> return (map (\f -> if f <= p then 1 else 0) fs))

initField' :: Int -> Int -> Float -> IO [Int]
initField' x y p = do fs <- randFloats (x * y)
                      return (map (\f -> if f <= p then 1 else 0) fs)

randFloats :: Int -> IO [Float]
randFloats 0 = return []
randFloats n = (randFloats (n-1)) >>= (\fs -> (getStdRandom (random) :: IO Float) >>= (\f -> return (f : fs)))

randFloats' :: Int -> IO [Float]
randFloats' 0 = return []
randFloats' n = do fs <- randFloats (n-1)
                   f <- (getStdRandom (random) :: IO Float)
                   return (f: fs)
                   
performSimSteps :: [Int] -> Int -> IO [[Int]]
performSimSteps field 0 = return [field]
performSimSteps field n = return [field]

simStep :: [Int] -> [Int]
simStep field = field



genCoords :: [(Int, Int)]
genCoords  = [ (x, y) | x <- [1..xCells], y <- [1..yCells] ]

coordsToIndex :: (Int, Int) -> Int
coordsToIndex (x, y) = ((x-1) * xCells) + y

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = map (\(nx, ny) -> (x+nx, y+ny)) neighbourhood



calcFieldRatio :: IO [Int] -> IO Float
calcFieldRatio field = do fs <- field
                          s <- return (sum fs)
                          l <- return (length fs)
                          return (((fromIntegral s) / (fromIntegral l)) :: Float)

{- THIS IS FOR TESTING PURPOSES ONLY -}
randomBools :: (RandomGen g) => Int -> Float -> g -> ([Bool], g)
randomBools 0 p g = ([b], g)
  where rb = randomBool p g
        b = fst rb
        g' = snd rb
randomBools n p g = ([b] ++ recBools, recGen)
                     where rb = randomBool p g
                           b = fst rb
                           g' = snd rb
                           recCall = (randomBools (n-1) p g')
                           recBools = fst recCall
                           recGen = snd recCall

randomBool :: RandomGen g => Float -> g -> (Bool, g)
randomBool p g = (b, g')
  where r = randomR (0.0, 1.0) g
        value = fst r
        g' = snd r
        b = value > p
