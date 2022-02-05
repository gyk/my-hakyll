#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import GHC.IO.Encoding
import System.FilePath (takeDirectory, (</>))
import Text.Pandoc.Extensions (enableExtension)
import Text.Pandoc.Options ( WriterOptions(..)
                           , ReaderOptions(..)
                           , HTMLMathMethod(MathJax, KaTeX)
                           , Extension(..)
                           , WrapOption(WrapNone))
import Hakyll


{-

Some example sites:

* https://github.com/yogsototh/yblog
* https://github.com/ian-ross/blog
* https://github.com/blaenk/blaenk.github.io
* http://www.gwern.net/hakyll.hs
* http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
* http://jdreaver.com/posts/2014-06-22-math-programming-blog-hakyll.html

-}


-- write in markdown or plain text
postPattern :: Pattern
postPattern = "posts/**.md" .||. "posts/**.txt"

simplePostCtx :: Context String
simplePostCtx = dateField "date" "%B %e, %Y" <> defaultContext

postCtxTags :: Tags -> Context String
postCtxTags tags = tagsField "tags" tags <> simplePostCtx


----------------------------------------------------------------

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8

  hakyll $ do
    tags <- buildTags postPattern (fromCapture "tags/*.html")
    match "posts/**.md" $ markdownPostBehaviorTags tags

    mapM_ (`match` staticBehavior)
      [ "js/**"
      , "pages/**"
      , "robots.txt"
      , complement "posts/**.metadata" .&&. "posts/**" ]

    match "css/**.css" $ route idRoute >> compile compressCssCompiler
    match "templates/*" $ compile templateCompiler

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**.md"
        let archiveCtx =
              listField "posts" simplePostCtx (return posts) <>
              constField "title" "Archives" <>
              defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
            >>= relativizeUrls

    -- homepage
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**.md"
        let indexCtx =
              listField "posts" simplePostCtx (return posts) <>
              constField "title" "Home" <>
              defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    match ("about.html" .||. "contact.html") $ do
      route idRoute
      compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged: &ldquo;" ++ tag ++ "&rdquo;"
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
              listField "posts" simplePostCtx (return posts) <>
              defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

----------------------------------------------------------------

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory (toFilePath ident) </> "index.html"

pandocCustomCompiler =
  let
    writerOptions =
      let mathExtensions =
            [ Ext_tex_math_dollars  -- $..$ or $$..$$
            , Ext_footnotes ]
          defaultExtensions = writerExtensions defaultHakyllWriterOptions
          newExtensions = foldr enableExtension defaultExtensions mathExtensions
      in
        defaultHakyllWriterOptions
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = MathJax ""
        , writerWrapText = WrapNone }
    readerOptions =
      let cjkExtensions = [ Ext_east_asian_line_breaks ]
          defaultExtensions = readerExtensions defaultHakyllReaderOptions
          newExtensions = foldr enableExtension defaultExtensions cjkExtensions
      in
        defaultHakyllReaderOptions
        { readerExtensions = newExtensions }
  in pandocCompilerWith readerOptions writerOptions

----------------------------------------------------------------

staticBehavior :: Rules ()
staticBehavior = route idRoute >> compile copyFileCompiler

markdownPageBehavior :: Rules ()
markdownPageBehavior = do
  route $ setExtension ".html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls

markdownPostBehaviorTags :: Tags -> Rules ()
markdownPostBehaviorTags tags = do
  route niceRoute
  compile $ pandocCustomCompiler
    >>= loadAndApplyTemplate "templates/post.html" (postCtxTags tags)
    >>= loadAndApplyTemplate "templates/default.html" (postCtxTags tags)
    >>= relativizeUrls


--markdownPostBehavior :: Rules ()
--markdownPostBehavior = do
--  route $ niceRoute
--  compile $ do
--    body <- getResourceBody
--    identifier <- getUnderlying
--    return $ renderPandoc (fmap (preFilters (toFilePath identifier)) body)
--    >>= applyFilter postFilters
--    >>= loadAndApplyTemplate "templates/default.html" yContext
--    >>= loadAndApplyTemplate "templates/boilerplate.html" yContext
--    >>= relativizeUrls
--    >>= removeIndexHtml
