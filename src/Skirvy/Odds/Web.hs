module Skirvy.Odds.Web
    ( handleRequest
    ) where

import Control.Monad (msum, liftM)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, nullDir, look,
                         serveDirectory, Browsing (DisableBrowsing), Response,
                         toResponse, serveFile, asContentType, ServerPart,
                         ServerPartT, HasRqData)
import Skirvy.Odds.Calculator (calculate, outcomeCounts)
import Text.Blaze.Html5 as H (Html, html, body, span, toHtml, table, tr, td)
import Text.Blaze.Html5.Attributes as A ()

handleRequest :: IO ()
handleRequest = simpleHTTP nullConf $ msum routes

routes :: [ServerPartT IO Response]
routes = [ dir "calculate" $ handleCalculateRequest,
           dir "foo" $ ok $ toResponse someHtml,
           dir "static" $ serveDirectory DisableBrowsing [] "./web-assets/" ,
           nullDir >> serveFile (asContentType "text/html") "./web-assets/index.html" ]

handleCalculateRequest :: ServerPart Response
handleCalculateRequest =
  do attacker <- lookInt "attacker"
     defender <- lookInt "defender"
     ok $ toResponse $ calculate attacker defender

lookInt :: (HasRqData m, Monad m) => String -> m Int
lookInt = liftM read . look

someHtml :: H.Html
someHtml = H.html $ H.body $ H.span $ H.toHtml "Foo"

aTable :: H.Html
aTable = H.table $ H.tr $ H.tr $ H.toHtml "Bar"
   where (winCounts, lossCounts) = outcomeCounts 5 3        

tableRow :: Int -> Int -> H.Html
tableRow a b = H.tr $  do 
                 H.td $ H.toHtml a
                 H.td $ H.toHtml b
                 H.td $ H.toHtml a
