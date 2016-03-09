--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase #-}
module PrevNextPost where
import Control.Applicative (Alternative (..))
import           Data.Char
import           Data.Maybe
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
import HakyllUtils

import Data.Time.Format (parseTime, defaultTimeLocale)
-- import System.Locale (defaultTimeLocale)
import Data.Time.Clock (UTCTime)

prevNextContext :: Pattern -> Context String
prevNextContext postsGlob = field "nextPost" (nextPostUrl postsGlob) <>
                            field "prevPost" (previousPostUrl postsGlob)

previousPostUrl :: Pattern -> Item String -> Compiler String
previousPostUrl postsGlob post = do
    let ident = itemIdentifier post
    posts <- getMatches postsGlob
    dates <- mapM (getItemUTC defaultTimeLocale) posts
    let sorted = sort $ zip dates posts
        (_, ordPosts) = unzip sorted  
    let ident' = itemBefore ordPosts ident
    case ident' of
        Just i -> (fmap (maybe empty $ toUrl) . getRoute) i
        Nothing -> empty

nextPostUrl :: Pattern -> Item String -> Compiler String
nextPostUrl postsGlob post = do
    let ident = itemIdentifier post
    posts <- getMatches postsGlob
    dates <- mapM (getItemUTC defaultTimeLocale) posts
    let sorted = sort $ zip dates posts
        (_, ordPosts) = unzip sorted  
    let ident' = itemAfter ordPosts ident
    case ident' of
        Just i -> (fmap (maybe empty $ toUrl) . getRoute) i
        Nothing -> empty

itemAfter' :: Eq a => [(a,b)] -> a -> Maybe b
itemAfter' xys x = do
    let (xs, ys) = unzip xys
    x' <- lookup x $ zip xs (tail xs)
    lookup x' $ zip xs ys

itemAfter :: Eq a => [a] -> a -> Maybe a
itemAfter xs x =
    lookup x $ zip xs (tail xs)

itemBefore' :: Eq a => [(a,b)] -> a -> Maybe b
itemBefore' xys x = do
    let (xs, ys) = unzip xys
    x' <- lookup x $ zip (tail xs) xs
    lookup x' $ zip xs ys

itemBefore :: Eq a => [a] -> a -> Maybe a
itemBefore xs x =
    lookup x $ zip (tail xs) xs


urlOfPost :: Item String -> Compiler String
urlOfPost =
    fmap (maybe empty $ toUrl) . getRoute . itemIdentifier
