module Skirvy.Odds.Web
    ( handleRequest
    ) where

import Control.Monad (msum)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, nullDir, look,
                         serveDirectory, Browsing (DisableBrowsing), Response,
                         toResponse, serveFile, asContentType, ServerPart)

handleRequest :: IO ()
handleRequest = simpleHTTP nullConf $ msum 
                  [ dir "foo" $ ok $ toResponse "Foo" , 
                    dir "calculate" $ handleCalculateRequest,
                    dir "static" $ serveDirectory DisableBrowsing ["index.html"] "./web-assets/" ,
                    nullDir >> serveFile (asContentType "text/html") "./web-assets/index.html" ]

handleCalculateRequest :: ServerPart Response
handleCalculateRequest =
  do attacker <- look "attacker"
     defender <- look "defender"
     ok $ toResponse $ "Trying to calculate for attacker: " ++ attacker ++ 
          " and defender " ++ defender

