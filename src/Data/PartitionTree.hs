{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
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
type Bounds b = [(b, b)]

data PartitionTree b a = Node (Bounds b) (MM.MultiMap (Bounds b) a) (Maybe (A.Array Int (PartitionTree b a))) deriving Show

empty :: Bounds b -> PartitionTree b a
empty bs = Node bs MM.empty Nothing

insert :: (Fractional b, Ord a, Ord b) => Bounds b -> a -> PartitionTree b a -> PartitionTree b a
insert bs x (Node nodeBs mm Nothing) = Node nodeBs (MM.insert bs x mm) Nothing
insert bs x (Node nodeBs mm (Just a)) = case getIndex bs nodeBs of
  Nothing -> Node nodeBs (MM.insert bs x mm) (Just a)
  Just n -> Node nodeBs mm (Just (modifyArray n (insert bs x) a))

fracture (Node nodeBs mm Nothing) = Node nodeBs MM.empty (newPartitions nodeBs) -- TODO: reinsert the items in mm into the new node.
fracture (Node nodeBs mm (Just a)) = Node nodeBs mm (Just a)

newPartitions :: Fractional b => Bounds b -> A.Array Int (PartitionTree b a)
newPartitions bs = A.listArray (0, length bs ^ 2 - 1) . map empty . possibilities . map bounds $ bs where
  bounds (l, r) = [(l, m), (m, r)] where
    m = (l + r) / 2

possibilities :: [[a]] -> [[a]]
possibilities = foldr (\xs xss -> concatMap (\x -> map (x :) xss) xs) [[]]



test = [(0,10), (0,20), (0,30)] :: Bounds Double


  
modifyArray :: (A.Ix i, A.IArray a e) => i -> (e -> e) -> a i e -> a i e
modifyArray k f a = a A.// [(k, f (a A.! k))]

getIndex :: (Fractional b, Ord b) => Bounds b -> Bounds b -> Maybe Int
getIndex item node = number <$> zipWithM getIndex' item node where
  number = foldr1 (\x y -> x + y * 2)
  getIndex' (l, r) (nl, nr)
    | r < nm = Just 0
    | nm <= l = Just 1
    | otherwise = Nothing where
      nm = (nl + nr) / 2