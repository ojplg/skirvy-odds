module Skirvy.Odds.Calculator
  ( partitionedOutcomes ) where

import qualified Data.Map.Lazy as M (Map, singleton, fromList, map, empty, keys, mapKeys,
                                     foldrWithKey, unionWith, elems, partitionWithKey)

data BattleCounter = BattleCounter { attacker :: Int, defender :: Int } deriving (Show, Eq, Ord)

ratioAsFloat :: Int -> Int -> Float
ratioAsFloat num den = (fromIntegral num) / (fromIntegral den)

{- |
  The 'partitionedOutcomes' function takes two integers:
  the first representing the number of attacking armies,
  and the second representing the number of defending 
  armies. The resultant tuple contains maps representing
  the percentage chances of each possible outcome assuming
  the battle is fought to completion. The first item in
  the tuple is the number of remaining armies for the 
  attacker, the second is the number of remaining armies
  for the defender.
-}
partitionedOutcomes :: Int -> Int -> (M.Map Int Float, M.Map Int Float)
partitionedOutcomes att def = (M.mapKeys remainder as, M.mapKeys remainder ds)
  where (as, ds) = partitionedOutcomes' att def

partitionedOutcomes' :: Int -> Int -> (M.Map BattleCounter Float, M.Map BattleCounter Float)
partitionedOutcomes' att def = M.partitionWithKey pred $ allOutcomes att def
                                 where pred bc _ = attackerWon bc

allOutcomes :: Int -> Int -> M.Map BattleCounter Float
allOutcomes att def = completeRounds $ applyOutcomes (BattleCounter att def) 1

completeRounds :: M.Map BattleCounter Float -> M.Map BattleCounter Float
completeRounds m = if all isResolved $ M.keys m then m
                     else completeRounds $ M.foldrWithKey myInsert M.empty m

myInsert :: BattleCounter -> Float -> M.Map BattleCounter Float -> M.Map BattleCounter Float
myInsert bc n m = M.unionWith (+) (applyOutcomes bc n) m

applyOutcomes :: BattleCounter -> Float -> M.Map BattleCounter Float
applyOutcomes bc r = if isResolved bc 
                        then M.singleton bc r
                        else M.map (* r) $ applyOutcomes' bc

applyOutcomes' :: BattleCounter -> M.Map BattleCounter Float
applyOutcomes' bc = M.fromList $ map (applyOutcome bc total) outcomes
  where outcomes = rollOutcomes (attackerDice $ attacker bc) (defenderDice $ defender bc)
        total = sum $ map snd outcomes

applyOutcome :: BattleCounter -> Int -> ((Int,Int),Int) -> (BattleCounter, Float)
applyOutcome (BattleCounter atr dfr) total ((dfrLost, atrLost), count) = 
                (BattleCounter (atr - atrLost) (dfr - dfrLost), ratioAsFloat count total)

isResolved :: BattleCounter -> Bool
isResolved bc = attackerWon bc || defenderWon bc

attackerWon :: BattleCounter -> Bool
attackerWon (BattleCounter _ 0) = True
attackerWon _                   = False

defenderWon :: BattleCounter -> Bool
defenderWon (BattleCounter 1 _) = True
defenderWon _                   = False

remainder :: BattleCounter -> Int
remainder bc = if attackerWon bc then attacker bc
                 else if defenderWon bc then defender bc
                 else error $ "Looked for remainder in unresolved battle " ++ show bc

defenderDice :: Int -> Int
defenderDice a = min a 2 

attackerDice :: Int -> Int
attackerDice a = min (a-1) 3

-- These outcomes were computed by a different program
-- and hard-coded here.
-- Arguments are the number of attacker dice and defender dice
-- Result is a tuple that has ((defender losses, attacker losses),count)
rollOutcomes :: Int -> Int -> [((Int,Int),Int)]
rollOutcomes 1 1 = [((0,1),21),((1,0),15)]
rollOutcomes 2 1 = [((0,1),91),((1,0),125)]
rollOutcomes 3 1 = [((0,1),441),((1,0),855)]
rollOutcomes 1 2 = [((0,1),161),((1,0),55)]
rollOutcomes 2 2 = [((0,2),581),((1,1),420),((2,0),295)]
rollOutcomes 3 2 = [((0,2),2275),((1,1),2611),((2,0),2890)]
rollOutcomes a d = error $ "Impossible combination: " ++ show a ++ ", " ++ show d

