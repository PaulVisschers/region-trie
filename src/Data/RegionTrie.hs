{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Data.RegionTrie where

import qualified Prelude as P
import Prelude hiding (lookup)

import qualified Data.Array.IArray as A
import qualified Data.MultiMap as MM

import Control.Applicative ((<$>), (<*>))
import Control.Monad

{-
  1D:
  nr <= r -> become left child of parent node
  l < nl -> become right child of parent node
  r < nm -> insert into left child
  nm <= l -> insert into right child
  otherwise -> insert into self
  
  2D:
  nrx <= rx
    nry <= ry -> become bottom-left child of parent node
    ly < nly -> become top-left child of parent node
    otherwise -> become *-left child of parent node
  lx < nlx
    ly < nly -> become top-right child of parent node
    nry <= ry -> become bottom-right child of parent node
    otherwise -> become *-right child of parent node
  rx < nmx
    ry < nmy -> insert into bottom-left child
    nmy <= ly -> insert into top-left child
    otherwise -> insert into self
  nmx <= lx
    nmy <= ly -> insert into top-right child
    ry < nmy -> insert into bottom-right child
    otherwise -> insert into self
  otherwise -> insert into self
-}
type Region k = [(k, k)]

data RegionTrie k a = Node (Region k) (MM.MultiMap (Region k) a) (Maybe (A.Array Int (RegionTrie k a))) deriving Show

empty :: Region k -> RegionTrie k a
empty ks = Node ks MM.empty Nothing

insert :: (Fractional k, Ord a, Ord k) => Region k -> a -> RegionTrie k a -> RegionTrie k a
insert ks x (Node nodeKs mm Nothing) = if MM.size mm' > 1 && MM.keyCount mm' > 1 then fracture node else node where -- Might want to fine tune the selection process for fracturing.
  mm' = MM.insert ks x mm
  node = Node nodeKs mm' Nothing
insert ks x (Node nodeKs mm (Just a)) = case getIndex ks nodeKs of
  Nothing -> Node nodeKs (MM.insert ks x mm) (Just a)
  Just n -> Node nodeKs mm (Just (modifyArray n (insert ks x) a))

insertList :: (Fractional k, Ord a, Ord k) => [(Region k, a)] -> RegionTrie k a -> RegionTrie k a
insertList ks t = foldr (uncurry insert) t ks

fracture (Node nodeKs mm Nothing) = insertList (MM.assocs mm) $ Node nodeKs MM.empty (Just $ newPartitions nodeKs)
fracture n = n

assocs :: RegionTrie k a -> [(Region k, a)]
assocs (Node nodeKs mm Nothing) = MM.assocs mm
assocs (Node nodeKs mm (Just subTries)) = concatMap assocs (A.elems subTries) ++ MM.assocs mm

-- Very inefficient and also refractures.
merge (Node nodeKs mm (Just subTries)) = insertList (concatMap assocs (A.elems subTries)) (Node nodeKs mm Nothing)
merge n = n

newPartitions :: Fractional k => Region k -> A.Array Int (RegionTrie k a)
newPartitions ks = A.listArray (0, length ks ^ 2 - 1) . map empty . possibilities . map bounds $ ks where
  bounds (l, r) = [(l, m), (m, r)] where
    m = (l + r) / 2

possibilities :: [[a]] -> [[a]]
possibilities = foldr (\xs xss -> concatMap (\x -> map (x :) xss) xs) [[]]

testTrie = insertList [
  ([(2,4), (6,7)], 1),
  ([(3,7), (1,2)], 2),
  ([(2,4), (6,7)], 3),
  ([(7,8), (9,9)], 4),
  ([(8,9), (9,9)], 5)
  ] testEmpty
testEmpty = empty test
test = [(0,10), (0,10)] :: Region Double

modifyArray :: (A.Ix i, A.IArray a e) => i -> (e -> e) -> a i e -> a i e
modifyArray k f a = a A.// [(k, f (a A.! k))]

getIndex :: (Fractional k, Ord k) => Region k -> Region k -> Maybe Int
getIndex item node = number <$> zipWithM getIndex' item node where
  number = foldl1 (\x y -> x * 2 + y)
  getIndex' (l, r) (nl, nr)
    | r < nm = Just 0
    | nm <= l = Just 1
    | otherwise = Nothing where
      nm = (nl + nr) / 2