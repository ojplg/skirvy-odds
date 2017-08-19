module Skirvy.Odds.Calculator
  ( calculate ) where

import qualified Data.Map.Lazy as M (Map, singleton, fromList, map, empty, keys,
                                     foldrWithKey, unionWith)

data BattleCounter = BattleCounter { attacker :: Int, defender :: Int } deriving (Show, Eq, Ord)

calculate :: Int -> Int -> String
calculate attacker defender = 
    "Trying to calculate for attacker " ++ (show attacker)
       ++ " and defender " ++ (show defender) ++ "\n"
       ++ show (allOutcomes attacker defender)

allOutcomes :: Int -> Int -> M.Map BattleCounter Int
allOutcomes att def = completeRounds $ applyOutcomes (BattleCounter att def) 0

completeRounds :: M.Map BattleCounter Int -> M.Map BattleCounter Int
completeRounds m = if all isResolved $ M.keys m then m
                     else completeRounds $ M.foldrWithKey myInsert M.empty m

myInsert :: BattleCounter -> Int -> M.Map BattleCounter Int -> M.Map BattleCounter Int
myInsert bc n m = M.unionWith (+) (applyOutcomes bc n) m

applyOutcomes :: BattleCounter -> Int -> M.Map BattleCounter Int
applyOutcomes bc n = if isResolved bc 
                        then M.singleton bc n
                        else M.map (+ n) $ applyOutcomes' bc

applyOutcomes' :: BattleCounter -> M.Map BattleCounter Int
applyOutcomes' bc = M.fromList $ map (applyOutcome bc) outcomes
  where outcomes = rollOutcomes (attackerDice $ attacker bc) (defenderDice $ defender bc)

applyOutcome :: BattleCounter -> ((Int,Int),Int) -> (BattleCounter, Int)
applyOutcome (BattleCounter atr dfr) ((dfrLost, atrLost), count) = 
                (BattleCounter (atr - atrLost) (dfr - dfrLost), count)

isResolved :: BattleCounter -> Bool
isResolved bc = attackerWon bc || defenderWon bc

attackerWon :: BattleCounter -> Bool
attackerWon (BattleCounter _ 0) = True
attackerWon _                   = False

defenderWon :: BattleCounter -> Bool
defenderWon (BattleCounter 1 _) = True
defenderWon _                   = False

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

