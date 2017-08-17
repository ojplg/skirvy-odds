module Skirvy.Odds.Web
    ( handleRequest
    ) where

import Control.Monad (msum)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, nullDir)

handleRequest :: IO ()
handleRequest = simpleHTTP nullConf $ msum [ dir "foo" $ ok "Foo Response",
                                             dir "bar" $ ok "Bar BAR BAR",
                                             nullDir >> ok "Home Page"]

