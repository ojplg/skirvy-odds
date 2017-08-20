module Skirvy.Odds.Web
    ( handleRequest
    ) where

import Control.Monad (msum, liftM)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, nullDir, look,
                         serveDirectory, Browsing (DisableBrowsing), Response,
                         toResponse, serveFile, asContentType, ServerPart,
                         ServerPartT, HasRqData)
import Skirvy.Odds.Calculator (partitionedOutcomes)
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
     ok $ toResponse $ renderOutcomes $ partitionedOutcomes attacker defender

renderOutcomes :: (M.Map Int Float, M.Map Int Float) -> H.Html
renderOutcomes (attackerWins, defenderWins) = asTable attackerWins >> asTable defenderWins

lookInt :: (HasRqData m, Monad m) => String -> m Int
lookInt = liftM read . look

asTable :: M.Map Int Float -> H.Html
asTable counts = H.table $ H.tr $ 
                   do H.th $ H.toHtml "Remainder"
                      H.th $ H.toHtml "Percentage"
                      rowsFromMap counts

rowsFromMap :: M.Map Int Float -> H.Html
rowsFromMap m = M.foldrWithKey (\i j rs -> rs >> tableRow i j) (H.toHtml "") m

tableRow :: Int -> Float -> H.Html
tableRow a b = H.tr $ do
                        H.td $ H.toHtml a
                        H.td $ H.toHtml $ formatFloat b

formatFloat :: Float -> String
formatFloat f = printf "%.4f" f
