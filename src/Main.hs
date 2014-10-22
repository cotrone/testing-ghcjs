{-# LANGUAGE QuasiQuotes #-}
-- # LANGUAGE OverloadedStrings #

module Main (
    main
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (documentGetBody, documentCreateElement)
import GHCJS.DOM.HTMLElement (htmlElementSetInnerHTML, htmlElementSetInnerText)
import GHCJS.DOM.Element (elementOnclick)
import GHCJS.DOM.HTMLParagraphElement
       (castToHTMLParagraphElement)
import GHCJS.DOM.Node (nodeAppendChild)
import GHCJS.DOM.EventM (mouseClientXY)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html
import Data.Text.Lazy (unpack)
import Text.Hamlet
import Data.Text hiding (unpack)

main = runWebGUI $ \webView -> do
  enableInspector webView
  Just doc <- webViewGetDomDocument webView
  Just body <- documentGetBody doc
  htmlElementSetInnerHTML body $ unpack . renderHtml $ htmlDoc ["Hello","123"] render
  elementOnclick body $ do
    (x,y) <- mouseClientXY
    liftIO $ do
      Just newParagraph <- fmap castToHTMLParagraphElement <$> documentCreateElement doc "p"
      htmlElementSetInnerText newParagraph $ "Click " ++ show (x,y) 
      nodeAppendChild body (Just newParagraph)
      return ()
  return ()

data MyRoute = Home

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = (pack "/home")


-- htmlDoc :: [String] -> t -> Markup
htmlDoc :: [String] -> HtmlUrl MyRoute
htmlDoc vs = [hamlet| 
  $forall v <- vs 
    <li> #{v} @{Home}
    ^{htmlSubDoc}  
|]

htmlSubDoc :: HtmlUrl MyRoute
htmlSubDoc = [hamlet|
<h6> Here is a sub template 

|]