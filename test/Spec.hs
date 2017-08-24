import Test.QuickCheck (quickCheck)
import Skirvy.Odds.Calculator (partitionedOutcomes)
import Data.Map.Lazy as M (elems)

main :: IO ()
main = quickCheck testResultsAddToOne

testResultsAddToOne :: (Int, Int) -> Bool
testResultsAddToOne (a,d) = (abs $ 1.0 - attPer - defPer) < 0.0001
  where (attMap, defMap) = partitionedOutcomes attCnt defCnt
        attPer = sum $ M.elems attMap
        defPer = sum $ M.elems defMap
        attCnt = ( a `mod` 250 ) + 2
        defCnt = ( d `mod` 250 ) + 1

