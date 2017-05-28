--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase #-}
module NestedCategories where
import           Data.Char
import           Data.Monoid
import qualified Data.Set as S
import           Hakyll
import           Text.Pandoc.Options
import           System.FilePath (takeBaseName, takeFileName, takeDirectory, joinPath, splitPath, replaceExtension)
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

setDirRoute :: String -> String -> Routes
setDirRoute folderName ext = customRoute (((folderName++"/")++) . takeFileName . toFilePath)

takeFileNameRoute :: String -> Routes
takeFileNameRoute ext =  customRoute (flip replaceExtension ext . takeFileName . toFilePath)

{-| keep the entire directory name, cutting off the first part (which will be posts/) -}
getNestedCategory :: MonadMetadata m => Identifier -> m [String]
getNestedCategory = return . return . joinPath . tail . splitPath . takeDirectory . toFilePath
--getCategory = return . return . takeBaseName . takeDirectory . toFilePath

buildNestedCategories' :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildNestedCategories' = buildTagsWith getNestedCategory


makePostTreeAndCategories :: MonadMetadata m => Pattern -> (String -> Identifier) -> m (FunTree [String] [Identifier], Tags)
makePostTreeAndCategories pat f = do
  categories <- buildNestedCategories' pat f
  let tm = tagsMap categories
  let ft = for' tm (([], M.empty)::FunTree [String] [Identifier]) (\(path, addresses) ft0 -> 
                                      let 
                                          path' = map removeTrailingSlash $ splitPath path
                                          is = tail $ inits path'
                                      in ft0 & foldIterate (\i m1 -> insertBranch (init i) i m1) is
                                             & insertNodes' path' (addresses))
  let dirs = M.keys (snd ft) :: [[String]]
  let tags = map (\x -> (joinPath x, getAllChildren x ft)) dirs
  return (ft, categories{tagsMap = tags})              

(.&):: (a -> b) -> (b -> c) -> (a -> c) 
(.&) = flip (.)
  
{- As in, MapReduce -}
reduce :: (Monoid m, Ord a, Ord m) => [(a, m)] -> [(a, m)]
reduce = M.toList . M.map mconcat . MM.toMap . MM.fromList

removeTrailingSlash :: String -> String
removeTrailingSlash = reverse . (\li -> if head li == '/' then tail li else li) . reverse

--type FunTree l b = (l, M.Map l (b,[l]))
compileTree :: Context String -> FunTree [String] [Identifier] -> Compiler String
compileTree ctx p@(rt, m) = do
  let (li, ls') = m M.! rt -- :: ([Identifier], [[String]])
  let ls = sortBy (\x y -> compare (map (map toLower) x) (map (map toLower) y)) ls'
  --note we must put `.&&. hasNoVersion`, otherwise it tries and fails to load the toc.
  listItemString <- loadAll $ ((foldl (.||.) "" (map (fromGlob . toFilePath) li)) .&&. hasNoVersion)
--  listItemTitles <- forM listItemString (flip getMetadataField "title" . itemIdentifier)
  --sort by first
--  let listItemStringSorted = map snd $ sortBy (\x y -> compare (map toLower $ fst x) (map toLower $ fst y)) $ zip listItemTitles listItemString
  listItemStringSorted <- sortByField (map toLower) "title" listItemString
--(flip loadAllSnapshots) "content"  $ foldl (.||.) "" (map (fromGlob . toFilePath) li)
  postItems <- applyTemplateList postItemTemplate ctx listItemStringSorted
  childrenListStrings <- mapM (compileTree ctx) (map (,m) ls)
  let childrenOutline = mconcat $ zipWith (\catPath str -> printf "<li><b>%s</b> %s </li>" (last catPath) str) ls childrenListStrings
  -- \catPath str -> "<li><b>"++(last catPath)++"</b>"++postItems++"</li>"
  return ("<ul class=\"collapsibleList\">"++childrenOutline++postItems++"</ul>")
