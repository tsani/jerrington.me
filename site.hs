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

  -- match "pages/index.md" $ do
  --   route $ constRoute "index.html"
  --   compile $ do
  --     pandocCompiler
  --       >>= loadAndApplyTemplate "templates/default.html" myDefaultContext
  --       >>= relativizeUrls

  match "pages/index.md" $ do
    route $ constRoute "index.html"
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let ctx = listField "posts" postCtx (return posts) <> myDefaultContext

      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "pages/*" $ do
    route $ setExtension "html" `composeRoutes` dropRoute 6
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

  -- STATIC RESOURCES ---------------------------------------------------------

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

-- | A route that drops a given number of characters from a path to
-- produce a route.
dropRoute :: Int -> Routes
dropRoute n = customRoute (drop n . toFilePath)
