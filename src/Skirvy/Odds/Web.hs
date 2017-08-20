module Skirvy.Odds.Web
    ( handleRequest
    ) where

import Control.Monad (msum, liftM)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, nullDir, look,
                         serveDirectory, Browsing (DisableBrowsing), Response,
                         toResponse, serveFile, asContentType, ServerPart,
                         ServerPartT, HasRqData)
import Skirvy.Odds.Calculator (calculate, outcomeCounts)
import Text.Blaze.Html5 as H (Html, html, body, span, toHtml, table, tr, td, th)
import Text.Blaze.Html5.Attributes as A ()
import Data.Map.Lazy as M (Map, foldrWithKey)
import Text.Printf (printf)

handleRequest :: IO ()
handleRequest = simpleHTTP nullConf $ msum routes

routes :: [ServerPartT IO Response]
routes = [ dir "calculate" $ handleCalculateRequest,
           dir "static" $ serveDirectory DisableBrowsing [] "./web-assets/" ,
           nullDir >> serveFile (asContentType "text/html") "./web-assets/index.html" ]

handleCalculateRequest :: ServerPart Response
handleCalculateRequest =
  do attacker <- lookInt "attacker"
     defender <- lookInt "defender"
     ok $ toResponse $ aTable (outcomeCounts attacker defender)

lookInt :: (HasRqData m, Monad m) => String -> m Int
lookInt = liftM read . look

aTable :: (M.Map Int (Int,Float), M.Map Int (Int,Float)) -> H.Html
aTable (winCounts, lossCounts) = H.table $ H.tr $ 
                                   do H.th $ H.toHtml "Remainder"
                                      H.th $ H.toHtml "Count"
                                      H.th $ H.toHtml "Percentage"
                                      rowsFromMap winCounts

rowsFromMap :: M.Map Int (Int,Float) -> H.Html
rowsFromMap m = M.foldrWithKey (\i j rs -> combine rs (tableRow i j)) (H.toHtml "") m

combine :: H.Html -> H.Html -> H.Html
combine a b = a >> b

tableRow :: Int -> (Int,Float) -> H.Html
tableRow a b = H.tr $ do
                        H.td $ H.toHtml a
                        H.td $ H.toHtml $ fst b
                        H.td $ H.toHtml $ formatFloat $ snd b

formatFloat :: Float -> String
formatFloat f = printf "%.3f" f
