module Utils where

import Graphics.Gloss.Interface.Pure.Game

import Control.Monad.State.Strict hiding (mapM,mapM_)
import Control.Monad.Reader hiding (mapM,mapM_)

import Data.Monoid

import qualified Data.Map as Map hiding (Map)
import Data.Map (Map)

import World

asState :: Reader s a -> State s a
asState rdr = runReader rdr <$> get

roundToIntPoint :: Point -> IntPt
roundToIntPoint (x,y) = (round (x/scaleFactor), round (y/scaleFactor))

mapSet :: Ord k => k -> Maybe v -> Map k v -> Map k v
mapSet k (Just v) = Map.insert k v
mapSet k Nothing  = Map.delete k

newtype AEndo a m = AEndo {appAEndo :: m (a -> a)}
instance Applicative m => Monoid (AEndo a m) where
        mempty = AEndo $ pure id
        AEndo f `mappend` AEndo g = AEndo $ (.) <$> f <*> g

atMulti :: (Applicative m,Foldable f,Ord k) =>
    f k -> (Maybe v -> m (Maybe v)) -> Map k v -> m (Map k v)
atMulti keys f mp = ($ mp) <$> appAEndo (foldMap (AEndo . alter) keys) where
    alter k = mapSet k <$> f (Map.lookup k mp)
