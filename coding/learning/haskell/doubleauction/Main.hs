{-# LANGUAGE TemplateHaskell #-}
import System.Random
import System.Random.Shuffle
import System.IO.Unsafe
import Data.List

-- << Agent data structure 
data Agent = Agent {
  agentid :: Int,
  optimism :: Float,
  cash :: Float,
  assets :: Float
} deriving (Show, Eq)

data Offering = Offering {
  offeringid :: Int,
  amount :: Float,
  price :: Float
} deriving (Show, Eq)

data Match = Match {
  matchingask :: Offering,
  matchingbid :: Offering
} deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn "Hello Jonathan!"

da :: Int -> Int -> Int -> [Agent]
da agentCount replications maxIterations = calculateMedian allReplAgents
  where
    newAgents = createAgents agentCount
    allReplAgents = performReplications newAgents replications maxIterations
   
calculateMedian :: [[Agent]] -> [Agent]
calculateMedian allReplAgents = foldl (\accumAgents repl ->
                                        map (\(accAgent, replAgent)
                                             -> Agent{ agentid = agentid accAgent, optimism = optimism accAgent, cash = ((cash accAgent) + (cash replAgent)) / 2.0, assets = ((assets accAgent) + (assets replAgent)) / 2.0 }) (zip accumAgents repl) ) (head allReplAgents) (tail allReplAgents)

performReplications :: [Agent] -> Int -> Int -> [[Agent]]
performReplications newAgents 0 maxIterations = []
performReplications newAgents n maxIterations = [replAgents] ++ recursionAgents 
  where
    replAgents = performTXs maxIterations newAgents
    recursionAgents = performReplications newAgents (n-1) maxIterations


createAgents :: Int -> [Agent]
createAgents n = foldl (\agents idx ->
                            let
                              a = newAgent idx h
                              h = fromIntegral(idx) / fromIntegral(n + 1)
                            in agents ++ [a]) [] [1..n] 

newAgent :: Int -> Float -> Agent
newAgent id h = Agent{ agentid = id, optimism = h, cash = 1.0, assets = 1.0 }

performTXs :: Int -> [Agent] -> [Agent]
performTXs 0 agents = agents                
performTXs n agents = case m of 
  Nothing -> performTXs (n - 1) agents
  Just match -> performTXs (n - 1) (executeMatch match agents)
  where 
    m = performRound agents

-- TODO: shuffle agents
performRound :: [Agent] -> Maybe Match
performRound agents = findMatch asks bids
  where
    asks = map (\a -> makeAsk a) shuffledAgents
    bids = map (\a -> makeBid a) shuffledAgents
    shuffledAgents = unsafePerformIO (shuffleM agents)

--shuffleAgents :: MonadRandom m [Agent] -> [Agent]
--shuffleAgents m agents = agents

findMatch :: [Maybe Offering] -> [Maybe Offering] -> Maybe Match
findMatch asks bids = case matchingTuple of
  Nothing -> Nothing
  Just (Just ask, Just bid) -> Just Match{ matchingask = ask, matchingbid = bid }
  where
    matchingTuple = find matchOfferingTup offeringTuples
    offeringTuples = [(a, b) | a <- asks, b <- bids ]
 
matchOfferingTup :: (Maybe Offering, Maybe Offering) -> Bool
matchOfferingTup (Nothing, _) = False
matchOfferingTup (_, Nothing) = False
matchOfferingTup (Just ask, Just bid)
  | bidId == askId = False
  | otherwise = bidPrice >= askPrice
  where
    askPrice = price ask
    bidPrice = price bid
    askId = offeringid ask
    bidId = offeringid bid
  
executeMatch :: Match -> [Agent] -> [Agent] 
executeMatch match agents = replaceAgent (replaceAgent agents newBuyer) newSeller 
  where
    newBuyer = buy tradingTuple buyer
    newSeller = sell tradingTuple seller
    tradingTuple = (tradingPrice, tradingAmount)
    tradingAmount = min askAmount bidAmount
    tradingPrice = tradingAmount * matchingPrice
    buyer = agents !! (bidderId - 1)
    seller = agents !! (askerId - 1)
    askerId = offeringid ask
    bidderId = offeringid bid
    askPrice = price ask
    bidPrice = price bid
    askAmount = amount ask
    bidAmount = amount bid
    matchingPrice = ( askPrice + bidPrice ) / 2.0
    ask = matchingask match
    bid = matchingbid match
    
replaceAgent :: [Agent] -> Agent -> [Agent]
replaceAgent agents newAgent = infrontAgent ++ [newAgent] ++ afterAgent
  where
    infrontAgent = init (fst splitAtAgent)
    afterAgent = snd splitAtAgent
    splitAtAgent = splitAt agentIndex agents
    agentIndex = agentid newAgent
    
makeAsk :: Agent -> Maybe Offering
makeAsk a
  | tradingamount > 0 = Just Offering{ offeringid = agentid a, amount = tradingamount, price = tradingprice }
  | otherwise = Nothing
    where
      tradingamount = min tradeableAssets 0.1
      tradingprice = randomRange limitPrice 1.0
      limitPrice = assetLimit a
      tradeableAssets = assets a
                         
makeBid :: Agent -> Maybe Offering
makeBid a
  | tradingprice > 0 = Just Offering{ offeringid = agentid a, amount = tradingamount, price = tradingprice }
  | otherwise = Nothing
    where
      tradingamount = 0.1
      tradingprice = min (cash a) (randomRange 0.2 limitPrice)
      limitPrice = assetLimit a

buy :: (Float, Float) -> Agent -> Agent
buy (price, amount) buyer = Agent{ agentid = buyerId, optimism = buyerOptimism, cash = cashAfterBuy, assets = assetsAfterBuy }
  where
    buyerId = agentid buyer
    buyerOptimism = optimism buyer
    cashAfterBuy = (cash buyer) - price
    assetsAfterBuy = (assets buyer) + amount
    
sell :: (Float, Float) -> Agent -> Agent
sell (price, amount) seller = Agent{ agentid = sellerId, optimism = sellerOpt, cash = cashAfterSell, assets = assetsAfterSell }
  where
    sellerId = agentid seller
    sellerOpt = optimism seller
    cashAfterSell = (cash seller) + price
    assetsAfterSell = (assets seller) - amount

-- TODO: problem getStdRandom returns IO monad => use unsafePerformIO
randomRange :: Float -> Float -> Float
randomRange lower upper = unsafePerformIO (getStdRandom (randomR (lower, upper))) 

assetLimit :: Agent -> Float
assetLimit a = h * 1.0 + (1.0 - h) * 0.2
  where
    h = optimism a
