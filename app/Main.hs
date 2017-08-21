module Main where

import Skirvy.Odds.Web (handleRequest)
import System.Directory

main :: IO ()
main = do putStrLn "Starting ... " 
          dotStuff <- listDirectory "."
          putStrLn "Contents of ."
          mapM_ putStrLn dotStuff
          tmpStuff <- listDirectory "/tmp"
          putStrLn "--------------------"
          putStrLn "Contents of tmp"
          mapM_ putStrLn tmpStuff
          handleRequest
