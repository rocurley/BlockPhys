{-# LANGUAGE TemplateHaskell #-}

module World
( Velocity(..)
, Trajectory(..)
, IntPt
, BlockType(..)
, BlockKey(..)
, blockLoc
, BlockVal(..)
, blockType
, cci
, Block(..)
, BlockMap(..)
, LinkKey(..)
, LinkDir(..)
, LinkVal(..)
, Link(..)
, LinkMap(..)
, Force(..)
, CConKey(..)
, CConVal(..)
, CCon(..)
, CConMap(..)
, Player(..)
, JumpStatus(..)
, World(..)
, playerWidth
, playerHeight
, blocks
, links
, cCons
, cCis
, player
, Direction(..)
, force0
, popCci
, pushCci
, linkedBlocks
, atMulti
) where

import Graphics.Gloss.Interface.Pure.Game

import qualified Data.Map as H hiding (Map)
import Data.Map (Map)
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict hiding (mapM,mapM_)
import Data.Monoid
import Data.Foldable

type IntPt = (Int,Int)

type Velocity = (Float,Float)

data Trajectory = Parabola Point Velocity Float deriving (Show,Eq,Ord)

data BlockType = Normal | Bedrock deriving (Eq,Ord,Show)
newtype BlockKey = BlockKey {_blockLoc :: IntPt} deriving (Eq,Ord,Show)
data BlockVal = BlockVal {_blockType :: BlockType, _cci :: Int} deriving (Eq,Ord,Show)
type Block = (BlockKey,BlockVal)
type BlockMap = Map BlockKey BlockVal

--I don't like that this can represent invalid states, but the validitiy of a link is
--pretty much tied to the global state, so I don't think it can be cromulently enforced
--by the type system.

data LinkKey = Link LinkDir IntPt deriving (Eq,Ord,Show)
data LinkDir = L2R | D2U deriving (Eq,Ord,Show)
data LinkVal = OffLink | OnLink Force deriving (Eq,Ord,Show)
type Link = (LinkKey,LinkVal)
type LinkMap  = Map LinkKey LinkVal


data Force = Force {_up :: Double, _right :: Double, _rotCCW :: Double} deriving (Eq,Ord,Show)
force0 = Force 0 0 0

data Direction = UpDir| DnDir | LfDir | RtDir deriving (Eq,Ord,Show)

type CConKey = Int
type CConVal = Int
type CCon = (CConKey,CConVal)
type CConMap = Map CConKey CConVal

data Player = Player {_playerLoc :: Point,_playerVel :: Velocity, _jumpStatus :: JumpStatus} 

data JumpStatus = Standing BlockKey | Jumping Float | Falling | NewlyFalling Float

data World = World {_blocks :: BlockMap,
                    _links :: LinkMap,
                    _cCons :: CConMap,
                    _cCis :: [Int],
                    _player :: Player}

makeLenses ''World
makeLenses ''BlockVal
makeLenses ''BlockKey
makeLenses ''Player

playerWidth = 0.4 :: Float
playerHeight = 0.8 :: Float

popCci :: CConVal -> State World Int
popCci grounded = do
    i:is <- use cCis
    cCis.=is
    return i

pushCci :: Int -> State World ()
pushCci i = cCis%=(i:)

linkedBlocks :: LinkKey -> (BlockKey,BlockKey)
linkedBlocks (Link L2R (x,y)) = (BlockKey (x,y), BlockKey (x+1,y))
linkedBlocks (Link D2U (x,y)) = (BlockKey (x,y), BlockKey (x,y+1))

newtype AEndo a m = AEndo {appAEndo :: m (a -> a)}
instance Applicative m => Monoid (AEndo a m) where
        mempty = AEndo $ pure id
        AEndo f `mappend` AEndo g = AEndo $ (.) <$> f <*> g

atMulti :: (Applicative m,Foldable f,Ord k) => f k -> (Maybe v -> m (Maybe v)) -> Map k v -> m (Map k v)
atMulti keys f mp = ($ mp) <$> appAEndo (foldMap (AEndo . alter) keys) where
    alter k = mapSet k <$> f (H.lookup k mp)

mapSet :: Ord k => k -> Maybe v -> Map k v -> Map k v
mapSet k (Just v) = H.insert k v
mapSet k Nothing = H.delete k