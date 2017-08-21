module Main where

import Skirvy.Odds.Web (handleRequest)
import System.Directory

main :: IO ()
main = do putStrLn "Starting ... " 
          stuff <- listDirectory "."
          mapM_ putStrLn stuff
          handleRequest
