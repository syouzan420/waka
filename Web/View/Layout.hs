module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext
import Web.Types
import Web.Routes
import Application.Helper.View

defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}
    {stylesheets}
    {scripts}
    <title>{pageTitleOrDefault "あそべのもり"}</title>
</head>
<body>
    <div class="container mt-4">
        {showlogin} <br> {showToOneUser} 
        <div style="text-align: right"><a href={WelcomeAction}>HOME</a></div>
        {renderFlashMessages}
        {inner}
    </div>
</body>
|]

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details

showToOneUser :: Html
showToOneUser = case currentUserOrNothing of
                  Just currentUser -> do
                    let cid = show$currentUser.id
                    if cid==userTeru || cid==userTeruOverThere
                       then [hsx| <a>Hello Teru!</a> |]
                       else [hsx| <a>Hello User!</a> |]
                  Nothing -> [hsx| <a>You are not User.</a> |]

showCurrentUser :: Html
showCurrentUser = case currentUserOrNothing of
                    Just currentUser -> do
                      let text = show$currentUser.id
                      [hsx| <a>{text}</a> |]
                    Nothing -> [hsx| <a>PLEASE LOGIN"</a> |]

showlogin :: Html
showlogin = 
  case currentUserOrNothing of
     Just _ -> [hsx| <a class="js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a> |]
     Nothing -> [hsx| <a href={NewSessionAction}>Login</a>|]

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap-5.2.1/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-3.6.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap-5.2.1/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]

devScripts :: Html
devScripts = [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="App"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="TODO"/>
    <meta property="og:description" content="TODO"/>
    {autoRefreshMeta}
|]
