module Main where

import Skirvy.Odds.Web (handleRequest)

main :: IO ()
main = putStrLn "Starting ... " >> handleRequest
