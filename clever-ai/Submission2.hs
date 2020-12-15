
{-#  LANGUAGE GeneralizedNewtypeDeriving  #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE DeriveGeneric  #-}
{-#  LANGUAGE ScopedTypeVariables  #-}

module Submission2 where
import Lib

  hiding (example1, example2, example3, lookupPlanet)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (unfoldr)
import Data.List
import Data.Maybe
import Text.Printf
import Control.DeepSeq
import GHC.Generics
import Data.Array

deriving instance (Integral Growth)
deriving instance (Enum Growth)
deriving instance (Real Growth)

data Strategy
  = Pacifist
  | ZergRush
  | PlanetRankRush
  | Skynet
  deriving (Enum, Bounded, Show, Read)

logic :: Strategy -> GameState -> AIState -> ([Order], Log, AIState)
logic strat gs ai
  = let logic' = case strat of
          Pacifist       -> pacifist
          ZergRush       -> zergRush
          PlanetRankRush -> planetRankRush
          Skynet         -> skynet
    in logic' gs ai {turn = turn ai + 1}

data AIState = AIState
  { turn       :: Turns
  , rushTarget :: Maybe PlanetId
  , pRanks     :: Maybe PlanetRanks
  , pEdges     :: Map PlanetId [(WormholeId, Wormhole)]
  , pTargets   :: Map PlanetId [((WormholeId, Wormhole), PlanetRank)]
  } deriving Generic

initialState :: AIState
initialState = AIState
  { turn       = 0
  , rushTarget = Nothing
  , pRanks     = Nothing
  , pEdges     = M.empty
  , pTargets   = M.empty
  }

type Log = [String]

pacifist :: GameState -> AIState -> ([Order], Log, AIState)
pacifist _ ai = ([], ["Do no harm."], ai)

enemyPlanet :: Planet -> Bool
enemyPlanet (Planet (Owned Player2) _ _) = True
enemyPlanet _                            = False

findEnemyPlanet :: GameState -> Maybe PlanetId
findEnemyPlanet (GameState ps _ _)
  = case M.toList (M.filter enemyPlanet ps) of
      []              -> Nothing
      ((epId, _) : _) -> Just epId

send :: WormholeId -> Maybe Ships -> GameState -> [Order]
send wId mShips st
  | not (ourPlanet planet)                 = []
  | isNothing mShips || ships > totalShips = [Order wId totalShips]
  | otherwise                              = [Order wId ships]
 where
  Wormhole (Source src) _ _      = lookupWormhole wId st
  planet@(Planet _ totalShips _) = lookupPlanet src st
  ships                          = fromJust mShips

shortestPath :: PlanetId -> PlanetId -> GameState
             -> Maybe (Path (WormholeId, Wormhole))
shortestPath src dst st
  = case filter ((== dst) . target) (shortestPaths st src) of
      [] -> Nothing
      (x : _) -> Just x

ourPlanet :: Planet -> Bool
ourPlanet (Planet (Owned Player1) _ _) = True
ourPlanet _ = False

ourPlanets :: GameState -> Planets
ourPlanets (GameState ps _ _) = M.filter ourPlanet ps

lookupWormhole :: WormholeId -> GameState -> Wormhole
lookupWormhole wId (GameState _ wormholes _)
  = wormholes M.! wId

lookupPlanet :: PlanetId -> GameState -> Planet
lookupPlanet pId (GameState planets _ _)
  = planets M.! pId

attackFromAll :: PlanetId -> GameState -> [Order]
attackFromAll targetId gs
  = concatMap attackFromAll' oPs
    where
      oPs = M.toList (ourPlanets gs)

      attackFromAll' :: (PlanetId, Planet) -> [Order]
      attackFromAll' (sourceId, _)
        | isNothing mPath = []
        | otherwise                  = send wId Nothing gs
        where
          mPath     = shortestPath sourceId targetId gs
          Path _ ws = fromJust mPath
          (wId, _)  = last ws

zergRush :: GameState -> AIState
         -> ([Order], Log, AIState)
zergRush gs (AIState turns mPId mPRs m ts)
 | mPId == Nothing || ourPlanet p
    = ([], [], AIState turns (findEnemyPlanet gs) mPRs m ts)
 | otherwise
    = (attackFromAll pId gs, [], AIState turns mPId mPRs m ts)
   where
     pId = fromJust mPId
     p   = lookupPlanet pId gs

newtype PageRank = PageRank Double
  deriving (Num, Eq, Ord, Fractional)

type PageRanks pageId = Map pageId PageRank

instance Show PageRank where
  show (PageRank p) = printf "%.4f" p

initPageRanks :: (Graph g e pageId, Ord pageId)
              => g -> PageRanks pageId
initPageRanks g = M.fromList [ (p, PageRank (1 / fromIntegral n))
                             | p <- ps ]
  where ps = vertices g
        n  = length ps

example1 :: [(String, String, Integer)]
example1 = [("a","b",1), ("a","c",1), ("a","d",1),
            ("b","a",1), ("c","a",1), ("d","a",1), ("c","d",1)]

-- Used to test initPageRank'
exampleInit :: Map Int String
exampleInit = M.fromList [(0, "a"), (1, "a"), (2, "a"), (3, "a"), (4, "a")]

initPageRank' :: Map pageId a -> PageRanks pageId
initPageRank' pMap = M.map (const pRank) pMap
  where
    pRank = PageRank (1 / fromIntegral (M.size pMap))

nextPageRank :: (Ord pageId, Edge e pageId, Graph g e pageId) =>
  g -> PageRanks pageId -> pageId -> PageRank
nextPageRank g pr i = (1 - d) / n + d * sum [ pr M.! j / t j
                                            | j <- s i ]
 where
  d   = 0.85
  n   = fromIntegral (length (vertices g))
  t j = fromIntegral (length (edgesFrom g j))
  s i = map source (edgesTo g i)

nextPageRanks :: Ord pageId => Graph g e pageId =>
  g -> PageRanks pageId -> PageRanks pageId
nextPageRanks g pr = M.mapWithKey (const . nextPageRank g pr) pr

pageRanks :: (Ord pageId, Graph g e pageId) => g -> [PageRanks pageId]
pageRanks g = iterate (nextPageRanks g) (initPageRanks g)

pageRank :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank g = pageRanks g !! 200

nextPageRank' :: (Ord pageId, Edge e pageId, Graph g e pageId) =>
  g -> PageRanks pageId -> PageRank -> pageId -> PageRank -> Maybe PageRank
nextPageRank' g pr k i pri
  | abs (pri - pri') < k  = Nothing
  | otherwise             = Just pri'
 where
   pri' = nextPageRank g pr i

nextPageRanks' :: Ord pageId => Graph g e pageId =>
  g -> PageRank -> PageRanks pageId -> Maybe (PageRanks pageId)
nextPageRanks' g k pr = case M.mapAccumWithKey nextPageRank'' True pr of
                           (True,  pr)  -> Nothing
                           (False, pr') -> Just pr'
  where
    nextPageRank'' converged i pri = case nextPageRank' g pr k i pri of
                            Nothing   -> (converged, pri)
                            Just pri' -> (False, pri')

pageRanks' :: (Ord pageId, Graph g e pageId)
  => g -> PageRank -> [PageRanks pageId]
pageRanks' g k = iterateMaybe (nextPageRanks' g k) (initPageRanks g)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

pageRank' :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank' = last . (take 200) . (flip pageRanks' k)
  where k = 0.0001

example2 :: GameState
example2 = GameState planets wormholes fleets where
  planets = M.fromList
    [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 7))
    , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 2))
    , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 3))
    , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 6))
    ]
  wormholes = M.fromList
    [ (WormholeId 0, Wormhole (Source 0) (Target 1) (Turns 1))
    , (WormholeId 1, Wormhole (Source 0) (Target 2) (Turns 1))
    , (WormholeId 2, Wormhole (Source 0) (Target 3) (Turns 1))
    , (WormholeId 3, Wormhole (Source 1) (Target 0) (Turns 1))
    , (WormholeId 4, Wormhole (Source 2) (Target 0) (Turns 1))
    , (WormholeId 5, Wormhole (Source 3) (Target 0) (Turns 1))
    , (WormholeId 6, Wormhole (Source 2) (Target 3) (Turns 1))
    ]
  fleets = []

newtype PlanetRank = PlanetRank Double
  deriving (Num, Eq, Ord, Fractional)

type PlanetRanks = Map PlanetId PlanetRank

instance Show PlanetRank where
  show (PlanetRank p) = printf "%.4f" p

initPlanetRanks :: GameState -> PlanetRanks
initPlanetRanks g = M.fromList [ (p, PlanetRank (1 / fromIntegral n))
                               | p <- ps ]
  where ps = vertices g
        n  = length ps

planetRank :: GameState -> PlanetRanks
planetRank g = planetRanks g !! 100

planetRanks :: GameState -> [PlanetRanks]
planetRanks g = iterate (nextPlanetRanks g) (initPlanetRanks g)

nextPlanetRanks :: GameState -> PlanetRanks -> PlanetRanks
nextPlanetRanks g pr = M.mapWithKey (const . nextPlanetRank g pr) pr

nextPlanetRank :: GameState -> PlanetRanks
               -> PlanetId -> PlanetRank
nextPlanetRank g@(GameState planets _ _) pr i =
 (1 - d) / n + d * sum [ pr M.! j * growth i / growths j
                       | j <- targets i ]
 where
  d   = 0.85
  n   = fromIntegral (length planets)

  growth :: PlanetId -> PlanetRank
  growth i  = (\(Planet _ _ g) -> fromIntegral g)
                                  (planets M.! i)
  targets :: PlanetId -> [PlanetId]
  targets = (map target) . (edgesFrom g)

  growths :: PlanetId -> PlanetRank
  growths = (foldl (\x -> (x +) . growth . source) 0) . (edgesTo g)

checkPlanetRanks :: PlanetRanks -> PlanetRank
checkPlanetRanks = sum . M.elems

planetRankRush :: GameState -> AIState
               -> ([Order], Log, AIState)
planetRankRush gs (AIState t r Nothing m ts)
  = planetRankRush gs (AIState t r (Just (planetRank gs)) m ts)
planetRankRush gs (AIState turns mPId mPRs@(Just pRs) m ts)
  | isNothing mPId || ourPlanet p
     = ([], [], AIState turns (findBestPlanet gs pRs) mPRs m ts)
  | otherwise
     = (attackFromAll pId gs, [], AIState turns mPId mPRs m ts)
    where
      pId = fromJust mPId
      p   = lookupPlanet pId gs

findBestPlanet :: GameState -> PlanetRanks -> Maybe PlanetId
findBestPlanet (GameState ps _ _) pRs
  = case M.toList (M.filter (not . ourPlanet) ps) of
      []  -> Nothing
      ps' -> Just (fst (foldl cmpPlanet (-1,-1) ps'))
    where
      cmpPlanet :: (PlanetId, PlanetRank) -> (PlanetId, a)
                -> (PlanetId, PlanetRank)
      cmpPlanet old@(pId, pR) (pId', _)
        | isNothing mPR = old
        | pR' > pR  = (pId', pR')
        | otherwise = old
        where
          mPR = M.lookup pId' pRs
          pR' = fromJust mPR

initGrowthRanks :: GameState -> PlanetRanks
initGrowthRanks g@(GameState ps _ _)
  = M.fromList [ (pId, 1 / growth pId)
                  | pId <- pIds ]
    where
      pIds = vertices g

      growth :: PlanetId -> PlanetRank
      growth i  = (\(Planet _ _ g) -> fromIntegral g)
                                      (ps M.! i)

growthRank :: GameState -> PlanetRanks
growthRank g = growthRanks g !! 100

growthRanks :: GameState -> [PlanetRanks]
growthRanks g = iterate (nextPlanetRanks g) (initGrowthRanks g)

--Get a list of targets and the percentage of ships to send to the target
skynetTargets :: GameState -> [(WormholeId, Wormhole)] -> PlanetRanks
              -> [((WormholeId, Wormhole), PlanetRank)]
skynetTargets gs@(GameState ps _ _) ws pRanks
--If only friendly neighbours distribute ships based on neighbour value
  | null ePs = map (\wp -> (wp, pRanks M.! (target wp) / tPR)) ws
--If there are enemy/neutral neighbours send ships to planets you can conquer
  | otherwise = map (\wp -> (wp, (fromIntegral (cost wp)) / v)) ts
  where
    ePs = filter (\w -> not (ourPlanet (ps M.! (target w)))) ws
    (Planet _ (Ships s) _) = ps M.! (source (head ws))
    tPR = foldl (\x wp -> x + pRanks M.! (target wp)) (fromIntegral 0) ws

--Use knapsack to get the maximum value planets you can conquer this turn and attack
    sack = map (\wp -> (wp, cost wp, pRanks M.! (target wp))) ePs
    (_, ts) = bknapsack sack (s `div` 2)
    v = foldl (\x t -> x + fromIntegral (cost t)) (fromIntegral 0) ts

--Get the cost of a wormhole to take it over this turn
    cost :: (WormholeId, Wormhole) -> Int
    cost wp@(wId, Wormhole _ _ (Turns turns))
      | enemyPlanet p' = (s + g * turns)
      | ourPlanet p'   = 0
      | otherwise      = s
      where
        p'@(Planet _ (Ships s) (Growth g)) = ps M.! (target wp)

bknapsack :: forall name weight value .
  (Ix weight, Ord weight, Num weight,
    Ord value, Num value) =>
  [(name, weight, value)] -> weight -> (value, [name])
bknapsack wvs c
  = table ! (len - 1, c)
    where
      len = length wvs

      table :: Array (Int, weight) (value, [name])
      table = tabulate ((0, 0), (len - 1, c)) bknapHelper

      bknapHelper :: (Int, weight) -> (value, [name])
      bknapHelper (0, c)
        | w > c     = (0, [])
        | otherwise = (v, [n])
          where
            (n, w, v) = wvs !! 0
      bknapHelper (i, c)
        | w > c         = vn
        | v + v' >= v'' = (v + v', n : ns')
        | otherwise     = vn
          where
            (n, w, v)   = wvs !! i
            (v', ns')   = table ! ((i - 1), (c - w))
            vn@(v'', _) = table ! ((i - 1), c)

skynet :: GameState -> AIState -> ([Order], Log, AIState)
skynet gs (AIState t rT Nothing pEs ts)
  = planetRankRush gs (AIState t rT (Just (growthRank gs)) pEs ts)
skynet gs@(GameState ps _ _) ai@(AIState turns _ _ _ _)
  | turns < 900 = foldr sendShips ([], [], ai) ourPs
  | otherwise   = zergRush gs ai
  where
    ourPs = M.toList(ourPlanets gs)
    sendShips :: (PlanetId, Planet) -> ([Order], Log, AIState)
              -> ([Order], Log, AIState)
    sendShips (pId, p) (ods, lg, cAi@(AIState t rT mPRs@(Just pRs) pEs ts))
      | isJust mTs && not (null rOds) = (rOds ++ ods, lg, cAi)
      | isNothing mWs           = (nOds ++ ods, lg, AIState t rT mPRs nPEs nTs)
      | otherwise               = (nOds ++ ods, lg, AIState t rT mPRs pEs nTs)
        where
          mWs   = M.lookup pId pEs
          nWs   = edgesFrom gs pId
          jWs   = if (isNothing mWs) then nWs else fromJust mWs
          mTs   = M.lookup pId ts
          nEWs  = filter (\w -> not (enemyPlanet (ps M.! (target w)))) jWs
          sTs   = skynetTargets gs (if turns < 250 then nEWs else jWs) pRs
          rOds  = concatMap orderShips (fromJust mTs)
          nOds  = concatMap orderShips sTs
          nPEs  = (M.insert pId nWs pEs)
          nTs   = (M.insert pId sTs ts)


          orderShips :: ((WormholeId, Wormhole), PlanetRank) -> [Order]
          orderShips ((wId, w), PlanetRank pcent)
            = send wId (Just (Ships (floor (pcent * dShips)))) gs
              where
                Planet _ (Ships ships) _ = p
                dShips                   = fromIntegral ships

deriving instance Generic PlanetRank
deriving instance Generic PageRank

instance NFData PageRank
instance NFData PlanetRank
