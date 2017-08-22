module Main where

import Skirvy.Odds.Web (handleRequest)
import System.Directory

main :: IO ()
main = putStrLn "Starting ... " >> handleRequest
