{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Map2D
( Map2D(..)
, rangeInc
, keys
, toList
, fromList
, iso
, atMulti
, singleton
, empty
, filter
, size
, lookup
, (!)
, findWithDefault
) where

import Prelude hiding (filter, lookup)
import qualified Prelude

import qualified Data.Map as Map hiding (Map)
import Data.Map (Map)

import Data.Maybe
import Data.Monoid

import qualified Control.Lens as Lens
import Control.Lens ((.~),(^.))

mapRangeInc :: Ord k => k -> k -> Map k v -> Map k v
mapRangeInc l u m = let
    (_,lv,upMap) = Map.splitLookup l m
    (midMap,uv,_) = Map.splitLookup u upMap
    in Map.alter (const lv) l $ Map.alter (const uv) u midMap

newtype Map2D k1 k2 v = Map2D{fromMap2D :: Map k1 (Map k2 v)}

rangeInc :: (Ord a, Ord b) => (a,b) -> (a,b) -> Map2D a b v -> Map2D a b v
rangeInc (xMin,yMin) (xMax,yMax) = Map2D . fmap (mapRangeInc yMin yMax) . mapRangeInc xMin xMax . fromMap2D

instance Functor (Map2D k1 k2) where
    fmap f = Map2D . fmap (fmap f) . fromMap2D

toList :: (Ord a, Ord b) => Map2D a b v -> [((a,b), v)]
toList (Map2D xMap) = do
  (x, yMap) <- Map.toList xMap
  (y, v) <- Map.toList yMap
  return ((x,y), v)

keys :: (Ord a, Ord b) => Map2D a b v -> [(a,b)]
keys = map fst . toList

fromList :: (Ord a, Ord b) => [((a,b), v)] -> Map2D a b v
fromList = Map2D . foldr (\ ((x, y), v) -> Map.insertWith (Map.union) x (Map.singleton y v)) Map.empty

instance (Show a, Show b, Ord a, Ord b, Show v) => Show (Map2D a b v) where
  show m = "toList " ++ show (toList m)

iso :: Lens.Iso' (Map2D k1 k2 v) (Map k1 (Map k2 v))
iso = Lens.iso fromMap2D Map2D

type instance Lens.Index (Map2D k1 k2 v) = (k1,k2)
type instance Lens.IxValue (Map2D k1 k2 v) = v
instance (Ord k1, Ord k2) => Lens.Ixed (Map2D k1 k2 v) where
  --ix (k1, k2) = isoMap2D.ix k1.ix k2
  ix (k1, k2) = iso. Lens.ix k1. Lens.ix k2
  {-# INLINE ix #-}

maybeMapAsMap :: Lens.Iso' (Maybe (Map b v)) (Map b v)
maybeMapAsMap = Lens.iso (fromMaybe Map.empty) (\ m -> if Map.null m then Nothing else Just m)

instance (Ord a, Ord b) => Lens.At (Map2D a b v) where
    at (a, b) = iso. Lens.at a.maybeMapAsMap. Lens.at b

instance Foldable (Map2D a b) where
  foldMap f = foldMap (foldMap f) . fromMap2D

instance Traversable (Map2D a b) where
  traverse f = fmap Map2D . traverse (traverse f) . fromMap2D

atMulti :: (Ord a, Ord b, Traversable t) => t (a,b) -> Lens.Traversal' (Map2D a b v) (Maybe v)
atMulti ks f map2D = let
    mkvs' = traverse (\ k -> (k,) <$> f (map2D^.Lens.at k)) ks
    mapSet :: (Ord a, Ord b) => ((a,b), Maybe v) -> Endo (Map2D a b v)
    mapSet (k,v) = Endo $ Lens.at k.~ v
    in (`appEndo` map2D) <$> foldMap mapSet <$> mkvs'

filter :: (Ord a, Ord b) => (v -> Bool) -> Map2D a b v -> Map2D a b v
filter f = fromList . Prelude.filter (f . snd) . toList

size :: (Ord a, Ord b) => Map2D a b v -> Int
size = length . toList

lookup :: (Ord a, Ord b) => (a, b) -> Map2D a b v -> Maybe v
lookup (a,b) (Map2D mapX) = do
  mapY <- Map.lookup a mapX
  Map.lookup b mapY

(!) :: (Ord a, Ord b) => Map2D a b v -> (a, b) -> v
map2D ! k = case lookup k map2D of
              Just v -> v
              Nothing -> error "Map2D.!: given key is not an element in the map"

findWithDefault :: (Ord a, Ord b) => v -> (a, b) -> Map2D a b v -> v
findWithDefault d k = fromMaybe d . lookup k

singleton :: (Ord a, Ord b) => (a, b) -> v -> Map2D a b v
singleton (a, b) v = Map2D $ Map.singleton a $ Map.singleton b v

empty :: (Ord a, Ord b) => Map2D a b v
empty = Map2D Map.empty
