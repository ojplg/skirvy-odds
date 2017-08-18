module Skirvy.Odds.Calculator
  ( calculate ) where

calculate :: Int -> Int -> String
calculate attacker defender = 
    "Trying to calculate for attacker " ++ (show attacker)
       ++ " and defender " ++ (show defender)
