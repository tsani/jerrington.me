{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ( (<>) )
import Hakyll
import Text.Pandoc
import System.FilePath ( splitFileName, (</>), (<.>) )

blogName = "Jacob Errington"

myDefaultContext
  = constField "blogName" blogName
  <> defaultContext

main :: IO ()
main = hakyll $ do
  -- PAGES --------------------------------------------------------------------

  match "pages/index.html" $ do
    route $ constRoute "index.html"
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let mostRecent = head posts

      let indexCtx =
              listField "posts" postCtx (return posts) <>
              myDefaultContext

      getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

  match "pages/*" $ do
    route $
      setExtension "html" `composeRoutes` customRoute (drop 6 . toFilePath)
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" myDefaultContext
      >>= relativizeUrls

  -- RSS FEED -----------------------------------------------------------------

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- fmap (take 10) . recentFirst
        =<< loadAllSnapshots "posts/*" "content"
      renderAtom feedConfig feedCtx posts

  -- POSTS --------------------------------------------------------------------

  match (foldr (.||.) (complement mempty) ["posts/*", "drafts/*"]) $ do
    route $ setExtension "html"
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/post.html"  postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "notes/*" $ do
    route $ setExtension "html"
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "school/msc/doc/read/*/notes.*" $ do
    route readingNotesRoute
    compile $ pandocMathCompiler
      >>= loadAndApplyTemplate "templates/notes.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  -- STATIC RESOURCES ---------------------------------------------------------

  match "school/msc/doc/read/*/*.pdf" $ do
    route schoolPdfRoute
    compile copyFileCompiler

  match "static/**" $ do
    route (dropRoute 7)
    compile copyFileCompiler

  -- TEMPLATES ----------------------------------------------------------------
  match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  myDefaultContext

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler
  = pandocCompilerWith readerOptions writerOptions where
    readerOptions = defaultHakyllReaderOptions
    writerOptions = defaultHakyllWriterOptions
      { writerHTMLMathMethod = MathJax ""
      }

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle = "Jacob Thomas Errington's blog"
  , feedDescription = "Just another functional programming blog."
  , feedAuthorName = "Jacob Thomas Errington"
  , feedAuthorEmail = "blog@mail.jerrington.me"
  , feedRoot = "https://jerrington.me"
  }

schoolPdfRoute :: Routes
schoolPdfRoute = customRoute (g . splitFileName . toFilePath) where
  g (_, f) = "pdf" </> f

-- | A route that drops a given number of characters from a path to
-- produce a route.
dropRoute :: Int -> Routes
dropRoute n = customRoute (drop n . toFilePath)

readingNotesRoute :: Routes
readingNotesRoute = customRoute (g . toFilePath) where
  g = notesDir . htmlExt . init . fst . splitFileName . drop (length pref)
  htmlExt = (<.> "html")
  notesDir = ("notes" </>)
  pref = "school/msc/doc/read/" :: String
