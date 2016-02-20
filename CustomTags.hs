--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase #-}
module CustomTags where
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

getTagsFrom :: MonadMetadata m => String -> Identifier -> m [String]
getTagsFrom name identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll ",") $ M.lookup name metadata

buildTagsFrom :: MonadMetadata m => String -> Pattern -> (String -> Identifier) -> m Tags
buildTagsFrom name = buildTagsWith (getTagsFrom name)

makeTagsList :: Context String -> Tags -> Compiler String
makeTagsList ctx t = do
  let tm = tagsMap t
  fmap mconcat $ mapM (buildOneTag ctx) tm
  
buildOneTag :: Context String -> (String, [Identifier]) -> Compiler String
buildOneTag ctx (s, li) = do
  comps <- loadAll ((foldl (.||.) "" $ map (fromGlob . toFilePath) li) .&&. hasNoVersion)
  items <- applyTemplateList postItemTemplate ctx comps
  return (printf "<p><b>%s</b></p>\n<ul>\n%s\n</ul>" s items)
