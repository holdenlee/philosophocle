{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XFlexibleContexts
 -XUndecidableInstances
 -XTupleSections
#-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FunTree where
import qualified Data.Map as M
import Control.Lens hiding (children)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.RWS
import Utilities
import Search
import MonadSupply

import Data.Tree
import Data.List

--I'm using this instead of fgl because it's more lightweight
type FunTree l b = (l, M.Map l (b,[l]))
--make this a newtype?

--ex. m b = [b]
insertNodes' :: (MonadPlus m, Ord l) => l -> m b -> FunTree l (m b) -> FunTree l (m b)
insertNodes' x ys (l, m) = 
    case M.lookup x m of
      Nothing -> (l, m & M.insert x (ys, []))
      Just _ -> (l, m & M.adjust (_1 %~ (`mplus` ys)) x)

--can also do for monoid
insertBranch :: (MonadPlus m, Ord l) => l -> l -> FunTree l (m b) -> FunTree l (m b)
insertBranch x x' (l,m) = 
    case M.lookup x m of
      Nothing -> (l, m & M.insert x (mzero, [x']))
      Just _ -> (l, m & M.adjust (_2 %~ (`union` [x'])) x)

getAllChildren :: (MonadPlus m, Ord l) => l -> FunTree l (m b) -> m b
getAllChildren x ft@(l, m) = 
    case M.lookup x m of
      Nothing -> mzero
      Just (b, li) -> b `mplus` msum (map (\x1 -> getAllChildren x1 ft) li)

type StateSupply f l c = StateT f (Supply l) c

--requires UndecidableInstances
instance (MonadSupply l s) => MonadSupply l (StateT f s) where
  peekS = (lift $ peekS)
  supply = (lift $ supply)
--should be a better way to do this... see Scraps.hs. @stackoverflow?

--Think a = Tree b
treeToFun' :: (Ord l, Treelike a b) => a -> StateSupply (FunTree l b) l ()
treeToFun' tree = do
  i <- supply
  let y = root tree
  modify (_2 %~ M.insert i (y,[]))
  indices <- forM (children tree)
             (\c -> do
                 a <- peekS
                 treeToFun' c
                 return a)
  modify (_2 %~ M.adjust (_2 %~ (const indices)) i)

treeToFun :: (Ord l, Treelike a b) => [l] -> a -> FunTree l b
treeToFun ls tree = runIdentity $ (flip evalSupply) ls $ (flip execStateT) (head ls, M.empty) $ treeToFun' tree

funToTree :: (Ord l, Treelike a b) => FunTree l b -> a
funToTree (i, m) =
  let (y, cs) = m M.! i
  in graft y (map (\c -> funToTree (c, m)) cs) 

compareFuns' :: (Ord l, Eq b) => RWS ((FunTree l b, FunTree l b)) [((l, b), (l, b))] (l,l) ()
compareFuns' = do
  ((_,m1),(_,m2)) <- ask
  (r1, r2) <- get
  let (y1,c1) = m1 M.! r1
  let (y2,c2) = m2 M.! r2
  if y1==y2
      then forM_ (zip c1 c2) (put >=> const compareFuns')
           {-
                 (\(ch1, ch2) ->
                   (put (ch1, ch2)) >> compareFuns')-}
      else tell [((r1,y1),(r2,y2))]

compareFuns :: (Ord l, Eq b) => (FunTree l b, FunTree l b) -> [((l,b),(l,b))]
compareFuns (f1, f2) = snd $ evalRWS compareFuns' (f1,f2) (fst f1, fst f2)

ftreeMap :: (l -> (b, [l]) -> (b,[l])) -> FunTree l b -> FunTree l b
ftreeMap f (h, t) = (h, M.mapWithKey f t)

testTree :: Tree Int
testTree = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]

funTreeChildren :: (Ord a) => FunTree a b -> [FunTree a b]
funTreeChildren (rt, m) =
  let
    (_, ls) = m M.! rt
  in
   map (,m) ls
