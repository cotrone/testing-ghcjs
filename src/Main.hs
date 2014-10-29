{-# LANGUAGE QuasiQuotes #-}
-- # LANGUAGE OverloadedStrings #

module Main (
    main
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (documentGetBody, documentGetHead, documentCreateElement, documentGetElementById)
import GHCJS.DOM.HTMLElement (htmlElementSetInnerHTML, htmlElementSetInnerText, castToHTMLElement)
import GHCJS.DOM.Element (elementOnclick, elementOnkeydown)
import GHCJS.DOM.HTMLParagraphElement
       (castToHTMLParagraphElement)
import GHCJS.DOM.Node (nodeAppendChild)
import GHCJS.DOM.EventM (mouseClientXY)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html
import Data.Text.Lazy (unpack)
import Text.Hamlet
import Data.Text hiding (unpack)
import GHCJS.DOM.HTMLDivElement
import GHCJS.DOM.UIEvent

main = runWebGUI $ \webView -> do
  enableInspector webView
  Just doc <- webViewGetDomDocument webView
  Just body <- documentGetBody doc
  Just dHead <- documentGetHead doc
  Just bDiv <- fmap castToHTMLElement <$> documentCreateElement doc "p"
  htmlElementSetInnerHTML dHead $ unpack . renderHtml $ htmlHead render
  htmlElementSetInnerHTML body $ unpack . renderHtml $ htmlSubDoc emailList  options render
  nodeAppendChild body (Just bDiv)
  Just el <- documentGetElementById doc "email-content"
  elementOnclick body $ do
    x <- mouseClientXY
    liftIO $ do 
      htmlElementSetInnerHTML (castToHTMLElement el) $ unpack . renderHtml $ renderEmail (emailList !! 2) sampleEmail2 render
      htmlElementSetInnerHTML body $ unpack . renderHtml $ htmlSubDoc emailList  options render
      -- print x
      putStrLn "You clicked a button!"
      return ()
  -- elementOnclick body $ do
  --   (x,y) <- mouseClientXY
  --   liftIO $ do
  --     putStrLn "Hello from Native Haskell"
  --     Just newParagraph <- fmap castToHTMLParagraphElement <$> documentCreateElement doc "p"
  --     htmlElementSetInnerText newParagraph $ "Clicked " ++ show (x,y)
  --     nodeAppendChild body (Just newParagraph)
  --     return ()
  return ()

data MyRoute = Home

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = (pack "/home")



htmlHead :: HtmlUrl MyRoute
htmlHead = [hamlet| 
<link rel="stylesheet" href="http://yui.yahooapis.com/pure/0.5.0/pure-min.css">
    
    <!--[if lte IE 8]>
        <link rel="stylesheet" href="css/layouts/email-old-ie.css">
    <![endif]-->
    <!--[if gt IE 8]><!-->
        <link rel="stylesheet" href="css/layouts/email.css">
    <!--<![endif]-->
|]

htmlSubDoc :: [Email] -> [(String, Int)] -> HtmlUrl MyRoute
htmlSubDoc emails options = [hamlet|
 <div class="content pure-g" id="layout">
        <div class="pure-u" id="nav">
            <a class="nav-menu-button" href="#">Menu

            <div class="nav-inner">
                <button class="primary-button pure-button">Compose

                <div class="pure-menu pure-menu-open">
                    <ul>
                        $forall option <- options
                          ^{renderSideOption option}
                        <li class="pure-menu-heading">Labels

                        <li>
                            <a href="#"><span class="email-label-personal"></span>Personal

                        <li>
                            <a href="#"><span class="email-label-work"></span>Work

                        <li>
                            <a href="#"><span class= "email-label-travel"></span>Travel

        <div class="pure-u-1" id="list">
          $forall email <- emails
            ^{renderEmailDesc email}
        

        <div class="pure-u-1" id="main">
            ^{renderEmail (emailList !! 0) sampleEmail}
                    

|]

options = [("Inbox", 2), ("Important", 1), ("Sent", 10), ("Drafts", 30), ("Trash", 0)]

renderSideOption :: (String, Int) -> HtmlUrl MyRoute
renderSideOption (name, count) = [hamlet|
<li>
  <a href="#">#{name} ^{renderSideOptionCount count}
|]

renderSideOptionCount :: Int -> HtmlUrl MyRoute
renderSideOptionCount 0 = [hamlet| |]
renderSideOptionCount n = [hamlet| <span class="email-count">(#{n})|]

renderEmailDesc :: Email -> HtmlUrl MyRoute
renderEmailDesc (Email name subject desc) = [hamlet| 
<div class="email-item pure-g">
  <div class="pure-u"><img alt="Yahoo! News&#x27; avatar" class="email-avatar" height="64" src="img/common/ynews-avatar.png" width="64">

  <div class="pure-u-3-4">
        <h5 class="email-name">#{name}

        <h4 class="email-subject">#{subject}

        <p class="email-desc">#{desc}
|]


renderEmail :: Email -> HtmlUrl MyRoute -> HtmlUrl MyRoute
renderEmail (Email name subject desc) body = [hamlet|
<div class="email-content">
  <div class="email-content-header pure-g">
      <div class="pure-u-1-2">
          <h1 class="email-content-title">Hello from Toronto
  
          <p class="email-content-subtitle">
            From Tilo Mitra at <span> 3:56pm, April 3, 2012
                    

                <div class="email-content-controls pure-u-1-2">
                  <button class="secondary-button pure-button">Reply
                  <button class="secondary-button pure-button">Forward
                  <button class="secondary-button pure-button">Move to
                    
                
                <div class="email-content-body" id="email-content">
                  ^{body}
|]


emailList = [Email "Eric Ferraiuolo" "Re: Pull Requests" "Hey, I had some feedback for pull request #51. We should center the menu so it looks better on mobile."
            ,Email "YUI Library" "You have 5 bugs assigned to you" "Duis aute irure dolor in reprehenderit in voluptate velit essecillum dolore eu fugiat nulla."
            ,Email "Reid Burke" "Re: Design Language" "Excepteur sint occaecat cupidatat non proident, sunt in culpa."
            ,Email "Andrew Wooldridge" "YUI Blog Updates?" "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip."
            ,Email "Yahoo! Finance" "How to protect your finances from winter storms" "Mauris tempor mi vitae sem aliquet pharetra. Fusce in dui purus, nec malesuada mauris."
            ,Email "Yahoo! News" "Summary for April 3rd, 2012" "We found 10 news articles that you  may like."]


sampleEmail = [hamlet|
<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\n
<p>Duis aute irure dolor in reprehenderit in voluptate velit essecillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
<p>Aliquam ac feugiat dolor. Proin mattis massa sit amet enim iaculis tincidunt. Mauris tempor mi vitae sem aliquet pharetra. Fusce in dui purus, nec malesuada mauris. Curabitur ornare arcu quis mi blandit laoreet. Vivamus imperdiet fermentum mauris, ac posuere urna tempor at. Duis pellentesque justo ac sapien aliquet egestas. Morbi enim mi, porta eget ullamcorper at, pharetra id lorem.
<p>Donec sagittis dolor ut quam pharetra pretium varius in nibh. Suspendisse potenti. Donec imperdiet, velit vel adipiscing bibendum, leo eros tristique augue, eu rutrum lacus sapien vel quam. Nam orci arcu, luctus quis vestibulum ut, ullamcorper ut enim. Morbi semper erat quis orci aliquet condimentum. Nam interdum mauris sed massa dignissim rhoncus.
<p>Regards,<br>Tilo
|]

sampleEmail2 = [hamlet|
<p>This is a third email
|]

data Email = Email {
  name :: String
, subject :: String
, desc :: String
} deriving (Eq, Show)