module Skirvy.Odds.Web
    ( handleRequest
    ) where

import Control.Monad (msum, liftM)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, nullDir, look,
                         serveDirectory, Browsing (DisableBrowsing), Response,
                         toResponse, serveFile, asContentType, ServerPart,
                         HasRqData)

handleRequest :: IO ()
handleRequest = simpleHTTP nullConf $ msum 
                  [ dir "calculate" $ handleCalculateRequest,
                    dir "static" $ serveDirectory DisableBrowsing ["index.html"] "./web-assets/" ,
                    nullDir >> serveFile (asContentType "text/html") "./web-assets/index.html" ]

handleCalculateRequest :: ServerPart Response
handleCalculateRequest =
  do attacker <- lookInt "attacker"
     defender <- lookInt "defender"
     ok $ toResponse $ doCalculation attacker defender

lookInt :: (HasRqData m, Monad m) => String -> m Int
lookInt = liftM read . look

doCalculation :: Int -> Int -> String
doCalculation attacker defender = 
    "Trying to calculate for attacker " ++ (show attacker)
       ++ " and defender " ++ (show defender)
