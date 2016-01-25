--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import           Text.Pandoc


--------------------------------------------------------------------------------

blogName = "Jacob Errington"

myDefaultContext
    = constField "blogName" blogName
    <> defaultContext

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "font/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/pdfs/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["about.md", "projects.md"] $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" myDefaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "notes/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let mostRecent = head posts

            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    myDefaultContext


            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    myDefaultContext

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler
    = pandocCompilerWith readerOptions writerOptions where
        readerOptions = defaultHakyllReaderOptions
        writerOptions = defaultHakyllWriterOptions
            { writerHTMLMathMethod = MathJax ""
            }
