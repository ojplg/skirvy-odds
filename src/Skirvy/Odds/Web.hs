module Skirvy.Odds.Web
    ( handleRequest
    ) where

import Control.Monad (msum)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, nullDir, 
                         serveDirectory, Browsing (DisableBrowsing),
                         toResponse, serveFile, asContentType)

handleRequest :: IO ()
handleRequest = simpleHTTP nullConf $ msum 
                  [ dir "foo" $ ok $ toResponse "Foo" , 
                    dir "static" $ serveDirectory DisableBrowsing ["index.html"] "./web-assets/" ,
                    nullDir >> serveFile (asContentType "text/html") "./web-assets/index.html" ]
