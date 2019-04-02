{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mappend)
import GHC.Exts (Any)
import Data.Aeson
import Data.List
import Hakyll
import Hakyll.Core.Identifier
import Hakyll.Core.Metadata
import Hakyll.Core.UnixFilter
import System.FilePath
import System.Environment
import System.Process
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
-- | Monadic version of concatMap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

tailwind :: Compiler (Item String)
tailwind = do
  fp <- getResourceFilePath
  tw <- unsafeCompiler $
    readCreateProcess (proc "node_modules/.bin/tailwind" ["build", fp]) ""
  makeItem tw

stringItems :: [Pattern]
stringItems =
  [ "posts/*"
  , "*.html"
  , "*.rst"
  , "*.markdown"
  ]

templateItems :: [Pattern]
templateItems =
  [ "templates/*"
  ]

{-
  How to use purgeCss with critical

  - Generate html & css without running purgeCss
  - Run purgecss
  - Run critical against purgecss output. Output
    both versions of the html.
  - Use cookies
-}

purgeCss :: Item String -> Compiler (Item String)
#ifdef PRODUCTION
purgeCss x = do
  let strPats = map (.&&. hasNoVersion) stringItems
  let tplPats = map (.&&. hasNoVersion) templateItems

  items <- concatMapM loadAll strPats
  tpls <- concatMapM loadAll tplPats

  let fps = concat
        [ map (toFilePath . itemIdentifier) (items :: [Item String])
        , map (toFilePath . itemIdentifier) (tpls :: [Item Template])
        ]
  unsafeCompiler $ mapM_ (putStrLn . ("purge run on: " ++)) fps
  TmpFile fp <- newTmpFile "-purgecss.css"
  withItemBody (unsafeCompiler . writeFile fp) x
  unsafeCompiler $ do
    setEnv "TEMP_CSS_PATH" fp
    r <- readCreateProcess (proc "node_modules/.bin/purgecss" $
                             [ "-c"
                             , "purgecss.config.js"
                             , "--css"
                             , fp
                             , "-o"
                             , takeDirectory fp
                             ] ++ fps) ""
    putStrLn r
  cssOut <- unsafeCompiler $ readFile fp
  pure $ itemSetBody cssOut x
#else
purgeCss = return
#endif

csso :: Item String -> Compiler (Item String)
csso = withItemBody (unixFilter "node_modules/.bin/csso" [])

criticalCssInline :: Item String -> Compiler (Item String)
#ifdef PRODUCTION
criticalCssInline str = do
  (_css :: Item String) <- load "css/default.css"
  withItemBody (unixFilter "node_modules/.bin/critical" ["-c", toFilePath "_site/css/default.css", "-i"]) str
#else
criticalCssInline = return
#endif

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile $ do
          tailwind >>= purgeCss >>= csso

    match "css/*" $ version "full" $ do
        route $ setExtension ".full.css"
        compile $ do
          tailwind >>= csso

    let pandocs = pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile pandocs

    match (fromList ["about.rst", "contact.markdown"]) $ version "critical-css" $ do
        route   $ setExtension "critical.html"
        compile (pandocs >>= criticalCssInline)

    let posts = pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ posts

    match "posts/*" $ version "critical-css" $ do
        route $ setExtension "critical.html"
        compile $ posts >>= criticalCssInline

    let archive = do
          posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
          let archiveCtx =
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Archives"            `mappend`
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls

    match "manifest.json" $ do
      route idRoute
      compile copyFileCompiler
    match "workbox-config.js" $ do
      route (constRoute "sw.js")
      compile $ do
        sw <- unsafeCompiler $ do
          r <- readCreateProcess (proc "node_modules/.bin/workbox" ["generateSW", "workbox-config.js"]) ""
          putStrLn r
          readFile "sw.js"
        ident <- getUnderlying
        pure $ Item ident sw

    create ["archive.html"] $ do
        route idRoute
        compile $ archive

    create ["archive.html"] $ version "critical-css" $ do
        route $ setExtension "critical.html"
        compile $ archive >>= criticalCssInline

    let index = do
          posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
          let indexCtx =
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Home"                `mappend`
                  defaultContext

          getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile index

    match "index.html" $ version "critical-css" $ do
        route $ setExtension "critical.html"
        compile (index >>= criticalCssInline)

    match "templates/*" $ compile templateBodyCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
