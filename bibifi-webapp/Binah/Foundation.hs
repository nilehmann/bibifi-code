module Binah.Foundation where

import Prelude
import Model (Principal)
import LMonad.Label.DisjunctionCategory (DCLabel)
import Yesod (HtmlUrl, Html, WidgetT, HandlerT, whamlet, julius, hamlet)
import Foundation.App
import Binah.Infrastructure (liftT, TaggedT)
import Foundation (LayoutData)
import Settings.StaticFiles
import Database.Persist (Entity(Entity))

-- defaultLayout :: TaggedT (DCLabel Principal) (WidgetT App IO) () -> TaggedT (DCLabel Principal) (HandlerT App IO) Html
-- defaultLayout = customLayout Nothing

-- customLayout :: LayoutData -> TaggedT (DCLabel Principal) (WidgetT App IO) () -> TaggedT (DCLabel Principal) (HandlerT App IO) Html
-- customLayout d widget = do
--         -- master <- getYesod
--         -- mmsg <- getMessage

--         nav <- liftT $ navbar d

--         -- We break up the default layout into two components:
--         -- default-layout is the contents of the body tag, and
--         -- default-layout-wrapper is the entire page. Since the final
--         -- value passed to hamletToRepHtml cannot be a widget, this allows
--         -- you to use normal widget features in default-layout.

--         mmsg <- liftT getMessage
--         let msg = case mmsg of
--               Nothing ->
--                 mempty
--               Just h ->
--                 h

--         -- pc <- widgetToPageContent widget
--             -- widget -- $(widgetFile "default-layout") -- $(hamletFile "templates/default-layout-wrapper.hamlet")
--         renderer <- liftT getUrlRenderParams
--         layout <- widgetToPageContent $ do
--             liftT $ do
--     --            addStylesheet $ StaticR css_bootstrap_min_css
--                 $(combineStylesheets 'StaticR
--                     [ css_bootstrap_min_css
--                     , css_langdon_css
--                     , css_jquery_ui_css
--                     , css_bootstrap_tokenfield_min_css
--                     , css_jquery_datetimepicker_min_css
--                     -- , css_bootstrap_theme_min_css
--                     ])
--                 (toWidget css)
--     --            addScript $ StaticR js_jquery_min_js
--     --            addScript $ StaticR js_bootstrap_min_js
--                 $(combineScripts 'StaticR
--                     [ js_jquery_min_js
--                     , js_bootstrap_min_js
--                     , js_jquery_ui_min_js
--                     , js_jquery_ui_min_js
--                     , js_jquery_datetimepicker_full_min_js
--                     , js_bootstrap_tokenfield_js
--                     -- , js_jquery_validate_min_js
--                     ])
--                 -- Analytics code.
--                 unless development $ toWidget [julius|
--                         (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
--                         (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
--                         m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
--                         })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

--                         ga('create', 'UA-52699115-1', 'auto');
--                         ga('send', 'pageview');
--                     |]
--             [whamlet|
--                 <div class="wrapper">
--                     <noscript>
--                         <div class="container">
--                             <div class="alert alert-warning">
--                                 <strong>Warning!</strong> This web site requires javascript.
--                     #{msg}
--                     ^{nav renderer}
--                     <div id="main" role="main">
--                         <div class="container">
--                             ^{widget}
--                     <div class="push container">
--                 <footer>
--                     <div class="container">
--                         <ul>
--                             <li>
--                                 <a href=@{SupportR}>
--                                     Support
--                             <li class="seperator">
--                             <li>
--                                 <a href=@{FeedbackR}>
--                                     Feedback
--                             <li class="seperator">
--                             <li>
--                                 <a href=@{ContestsR}>
--                                     Past Contests
--                 \<!-- Prompt IE 6 users to install Chrome Frame. Remove this if you want to support IE 6.  chromium.org/developers/how-tos/chrome-frame-getting-started -->
--                 \<!--[if lt IE 7 ]>
--                     <script src="http://ajax.googleapis.com/ajax/libs/chrome-frame/1.0.3/CFInstall.min.js">
--                     <script>
--                         window.attachEvent('onload',function(){CFInstall.check({mode:'overlay'})})
--                 \<![endif]-->
--             |]

--         lLift $ giveUrlRenderer [hamlet|$newline never
-- \<!doctype html>
-- \<!--[if lt IE 7]> <html class="no-js ie6 oldie" lang="en"> <![endif]-->
-- \<!--[if IE 7]>    <html class="no-js ie7 oldie" lang="en"> <![endif]-->
-- \<!--[if IE 8]>    <html class="no-js ie8 oldie" lang="en"> <![endif]-->
-- \<!--[if gt IE 8]><!-->
-- <html class="no-js" lang="en"> <!--<![endif]-->
--     <head>
--         <meta charset="UTF-8">

--         <title>#{pageTitle layout}
--         <meta name="description" content="The Build It Break It Fix It programming contest challenges participants to develop secure, but efficient software.">
--         <meta name="author" content="UMD, Department of Computer Science, PLUM, James Parker, Andrew Ruef, Michael Hicks">
--         <meta name="viewport" content="width=device-width,initial-scale=1">

--         ^{pageHead layout}

--         \<!--[if lt IE 9]>
--         \<script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
--         \<![endif]-->

--         <script>
--           document.documentElement.className = document.documentElement.className.replace(/\bno-js\b/,'js');

--     <body>
--         ^{pageBody layout}
-- |]

-- -- Layout for navigation bar.
-- -- TODO: check current path and site..
-- navbar :: LayoutData -> Handler (HtmlUrl (Route App))
-- navbar contest = do
--     mauth <- maybeAuth
--     participantLinks <- runLHandler $ participantNav contest mauth
--     contestLinks <- case contest of
--         Just (Entity _ c) ->
--             let url = contestUrl c in
--             return [hamlet|
--                 <li>
--                     <a href=@{SpecificAnnouncementsR url}>
--                         ANNOUNCEMENTS
--                 <li>
--                     <a href=@{SpecificScoreboardR url}>
--                         SCOREBOARD
--             |]
--         Nothing -> return [hamlet|
--             <li>
--                 <a href=@{AnnouncementsR}>
--                     ANNOUNCEMENTS
--             <li>
--                 <a href=@{ScoreboardR}>
--                     SCOREBOARD
--         |]
--     accountLinks <- case mauth of
--         Just (Entity _ u) ->
--             let adminNav =
--                   if userAdmin u then
--                     [hamlet|
--                         <li>
--                             <a href=@{AdminR}>
--                                 ADMIN
--                     |]
--                   else
--                     mempty
--             in
--             return [hamlet|
--                 ^{adminNav}
--                 <li>
--                     <a href=@{ProfileR}>
--                         PROFILE
--                 <li>
--                     <a href=@{AuthR LogoutR}>
--                         LOGOUT
--             |]
--         Nothing -> return [hamlet|
--             <li>
--                 <a href=@{AuthR LoginR}>
--                     LOGIN
--             <li>
--                 <a href=@{RegisterR}>
--                     REGISTER
--         |]

--     return [hamlet|
--         <div class="navbar" role="navigation">
--             <div class="container">
--                 <div class="collapse navbar-collapse navbar-ex1-collapse linksbar">
--                     <ul class="nav nav-pills navbar-right">
--                         <li>
--                             <a href=@{SponsorshipR}>
--                                 SPONSORSHIP OPPORTUNITIES
--                         ^{participantLinks}
--                         ^{contestLinks}
--                         <li>
--                             <a href=@{DetailsR}>
--                                 DETAILS
--                         <li class="seperator">
--                         ^{accountLinks}
--                 <div class="navbar-header">
--                     <button type="button" class="navbar-toggle" data-toggle="collapse" data-target=".navbar-ex1-collapse">
--                          <span class="sr-only">
--                              Toggle navigation
--                          <span class="icon-bar">
--                          <span class="icon-bar">
--                          <span class="icon-bar">
--                     <a class="brand" href="/">
--                         <img src=@{StaticR img_builditbreakitfixit_svg}>
--     |]

--     where
--         -- If logged in and participating in the contest, link to ContestParticipationR url
--         participantNav contest Nothing = return mempty
--         participantNav Nothing u = do
--             contestM <- defaultContest
--             case contestM of
--                 Nothing -> return mempty
--                 Just _ -> participantNav contestM u
--         participantNav (Just (Entity contestId contest)) (Just (Entity userId _)) = do
--             signedUp <- userIsSignedupForContest userId contestId

--             if signedUp then
--                 return [hamlet|
--                     <li>
--                         <a href="@{ContestParticipationR $ contestUrl contest}">
--                             PARTICIPANTS
--                 |]
--             else
--                 return mempty