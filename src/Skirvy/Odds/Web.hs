module Skirvy.Odds.Web
    ( handleRequest
    ) where

import Control.Monad (msum, liftM)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, nullDir, look,
                         serveDirectory, Browsing (DisableBrowsing), Response,
                         toResponse, serveFile, asContentType, ServerPart,
                         ServerPartT, HasRqData)
import Skirvy.Odds.Calculator (partitionedOutcomes)
import Text.Blaze as B (stringValue, stringTag, dataAttribute)
import Text.Blaze.Html5 as H (Html, html, body, span, toHtml, table, tr, td, th, h4, link, (!),
                              docTypeHtml, head, title, thead, tbody)
import Text.Blaze.Html5.Attributes as A (rel, href, class_)
import Data.Map.Lazy as M (Map, foldrWithKey, elems)
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
     ok $ toResponse $ page $ renderOutcomes $ partitionedOutcomes attacker defender

renderOutcomes :: (M.Map Int Float, M.Map Int Float) -> H.Html
renderOutcomes (attackerWins, defenderWins) = do
                                                H.h4 $ H.toHtml $ "Attacker Wins " ++ percentagesSum attackerWins
                                                asTable attackerWins 
                                                H.h4 $ H.toHtml $ "Defender Wins " ++ percentagesSum defenderWins
                                                asTable defenderWins

percentagesSum :: M.Map Int Float -> String
percentagesSum m = formatFloat $ sum $ M.elems m

lookInt :: (HasRqData m, Monad m) => String -> m Int
lookInt = liftM read . look

asTable :: M.Map Int Float -> H.Html
asTable counts = H.table ! A.class_ (B.stringValue "pure-table pure-table-bordered") $
                    do header
                       H.tbody $ rowsFromMap counts

rowsFromMap :: M.Map Int Float -> H.Html
rowsFromMap m = M.foldrWithKey (\i j rs -> rs >> tableRow i j) (H.toHtml "") m

header :: H.Html
header = H.thead $ H.tr $ do
           H.th $ H.toHtml "Remaining"
           H.th $ H.toHtml "Percentage"

tableRow :: Int -> Float -> H.Html
tableRow a b = H.tr $ do
                        H.td $ H.toHtml a
                        H.td $ H.toHtml $ formatFloat b

formatFloat :: Float -> String
formatFloat f = printf "%.4f" f

page :: H.Html -> H.Html
page content = H.docTypeHtml $ do
                  H.head $ do
                    H.title (H.toHtml "Skirvy Odds Results")
                    pureStylesheet
                  H.body $ do content

pureStylesheet :: H.Html
pureStylesheet = H.link ! A.rel (B.stringValue "stylesheet") 
                        ! A.href (B.stringValue "https://unpkg.com/purecss@1.0.0/build/pure-min.css")
                        ! B.dataAttribute (B.stringTag "crossorigin") (B.stringValue "anonymous")
                        ! B.dataAttribute (B.stringTag "integrity") (B.stringValue "sha384-nn4HPE8lTHyVtfCBi5yW9d20FjT8BJwUXyWZT9InLYax14RDjBj46LmSztkmNP9w")


