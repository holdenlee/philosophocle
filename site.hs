--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
import           Data.Monoid
import qualified Data.Set as S
import           Hakyll
import           Text.Pandoc.Options
import           System.FilePath (takeBaseName, takeDirectory, joinPath, splitPath, replaceExtension)
import           Control.Lens hiding (Context)
import           Control.Monad
import           Data.List
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import           Text.Printf
--import qualified Data.Tree as T
import Debug.Trace
import Utilities
import FunTree
import HakyllUtils
import NestedCategories
import TableOfContents
import CustomTags
import PrevNextPost

siteURL :: String
siteURL = "http://holdenlee.github.io/philosophocle"

{-| Main method -}
main :: IO ()
main = hakyll $ do
    --IMAGES: copy
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    --CSS: compress
    match ("css/*.css"
           .||. "*/css/*.css") $ do
        route   idRoute
        compile compressCssCompiler

    --JS: copy
    match ("js/*.js"
           .||. "*/js/*.js"
           .||. "MathJax/config/local/local.js") $ do
      route idRoute
      compile copyFileCompiler

    --TEMPLATES
    match "templates/*" $ compile templateCompiler
    
    --tags not actually used.
    --TAGS @ http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
    tags <- buildTags (postPattern .&&. hasNoVersion) (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
                  listField "posts" postCtx (return posts) <>
                  basicCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    --Authors
    authors <- makeTagsRules "author" (postPattern .&&. hasNoVersion) (fromCapture "writers/**.html") ("Posts by "++) "writers.html" "Writers"

    --POSTS
    match postPattern $ postRules authors tags
--(postPattern .&&. hasNoVersion)

    --TOP-LEVEL PAGES
    match "pages/*.md" $ pageRules 

    --INDEX
    match "index.html" $ do
        route idRoute
        compile $ do
            pandocMathCompiler
            posts <- recentFirst =<< loadAll (postPattern .&&. hasNoVersion)
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Philosophocle"   <>
                    basicCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
--                >>= loadAndApplyTemplate "templates/post.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    --ARCHIVE
    match "archive.html" $ do
        route idRoute
        compile $ do
            pandocMathCompiler
            posts <- chronological =<< loadAll (postPattern .&&. hasNoVersion)
            let arcCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archive"   <>
                    basicCtx

            getResourceBody
                >>= applyAsTemplate arcCtx
                >>= loadAndApplyTemplate "templates/post.html" arcCtx
                >>= loadAndApplyTemplate "templates/default.html" arcCtx
                >>= relativizeUrls

    --TOC for posts
    match postPattern $ compileTOCVersion

    --FEED
    atomCompiler "content" tags

makeTagsRules :: String -> Pattern -> (String -> Identifier) -> (String -> String) -> Identifier -> String -> Rules Tags
makeTagsRules name pat capt makeTitle pageName title = do 
    tags <- buildTagsFrom name pat capt
    tagsRules tags $ \tag pattern -> do
      let title = makeTitle tag -- "Posts by " ++ tag
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
                  listField "posts" postCtx (return posts) <>
                  basicCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    create [pageName] $ do
      route idRoute
      compile $ do
        let mapCtx = constField "title" title <> basicCtx
        s <- makeTagsList (blankTOCCtx <> postCtx) tags
        makeItem s
          >>= loadAndApplyTemplate "templates/post.html" mapCtx
          >>= loadAndApplyTemplate "templates/default.html" mapCtx
          >>= relativizeUrls

    return tags

postPattern :: Pattern
postPattern = "posts/**.md"

pageRules :: Rules ()
pageRules = do
  route $ takeFileNameRoute "html"
  defaultRules postCtx

-- !!!
postRules :: Tags -> Tags -> Rules ()
postRules authors tags = do
  route $ setExtension "html"
  defaultRules (tocCtx <> postCtxWithTags tags <> constField "isPost" "true")
--prevNextContext (postPattern .&&. hasNoVersion) <> 

defaultRules :: Context String -> Rules ()
defaultRules ctx = do
        compile $ pandocMathCompiler
            >>= saveSnapshot "bodyOnly"
            >>= return . fmap demoteHeaders -- h1 -> h2; h2 -> h3; etc
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            --before applying the website template, save a snapshot to send to Atom feed. saveSnapshot :: String -> Item a -> Compiler (Item a)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

{-| Math compiler. See http://jdreaver.com/posts/2014-06-22-math-programming-blog-hakyll.html. -}
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerReferenceLinks = True,
                          writerHtml5 = True,
                          writerHighlight = True,
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions


--------------------------------------------------------------------------------
basicCtx :: Context String
basicCtx = constField "siteURL" siteURL <> defaultContext

postCtx :: Context String
postCtx =
  --"%B %e, %Y" see https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time-Format.html for how to format date
    dateField "date" "%F" <>
    (teaserField "teaser" "bodyOnly") <>
    basicCtx

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <>
                       postCtx

{-| Feed configuration -}
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Philosophocle"
    , feedDescription = "A pinnacle of higher education"
    , feedAuthorName  = "Scholars of Philosophocle"
    , feedAuthorEmail = ""
    , feedRoot        = "http://holdenlee.github.io/philosophocle"
    }

{-| Compiler for feed -}
feedCompiler :: [Identifier] -> (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)) -> String -> Tags -> Rules ()
feedCompiler li renderer content tags =
  create li $ do
    route idRoute
    compile $ do
        let feedCtx = tocCtx <> (postCtxWithTags tags) <>
                bodyField "description"
        --take 10 most recent posts
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots (postPattern .&&. hasNoVersion) content
        renderer myFeedConfiguration feedCtx posts

{-| Why use Atom? http://nullprogram.com/blog/2013/09/23/ -}
atomCompiler = feedCompiler ["atom.xml"] renderAtom

--rssCompiler = feedCompiler ["rss.xml"] renderRss
