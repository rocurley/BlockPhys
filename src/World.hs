{-# LANGUAGE TemplateHaskell #-}

module World
( scaleFactor
, blockSize
, g
, jumpJerk
, vJump
, Velocity
, Trajectory(..)
, IntPt
, BlockType(..)
, BlockKey(..)
, blockLoc
, BlockVal(..)
, blockType
, cci
, Block
, BlockMap
, LinkKey(..)
, LinkDir(..)
, LinkVal(..)
, Link
, LinkMap
, Force(..)
, CConKey
, CConVal
, CCon
, CConMap
, Player(..)
, playerMovement
, PlayerMovement(..)
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
, linkedBlocks
, atMulti
, possibleLinks
, playerTrajectory
, playerLoc
) where

import Graphics.Gloss.Interface.Pure.Game

import qualified Data.Map as H hiding (Map)
import Data.Map (Map)
import Control.Lens
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

scaleFactor :: Float
scaleFactor = 80
blockSize :: Float
blockSize = 0.95
g :: Float
g = -1
jumpJerk :: Float
jumpJerk = -5
vJump :: Float
vJump = 5
vRunMax :: Float
vRunMax = 5


type IntPt = (Int,Int)

type Velocity = (Float,Float)

--Consider adding a "Patched Trajectory".
data Trajectory = Parabola Point Velocity Float |
                  RunTrajectory Point Float Float Float |
                  JumpTrajectory Point Velocity Float Float Float deriving (Show,Eq,Ord)
instance Arbitrary Trajectory where
  arbitrary = do
    n <- choose (0 :: Int ,2)
    case n of
      0 -> Parabola <$> arbitrary <*> arbitrary <*> arbitrary
      1 -> RunTrajectory <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      2 -> JumpTrajectory <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary  <*> arbitrary

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

force0 :: Force
force0 = Force 0 0 0

data Direction = UpDir| DnDir | LfDir | RtDir deriving (Eq,Ord,Show)

type CConKey = Int
type CConVal = Int
type CCon = (CConKey,CConVal)
type CConMap = Map CConKey CConVal

newtype Player = Player {_playerMovement :: PlayerMovement} deriving (Show) --Will have other things later

data PlayerMovement = Standing BlockKey Float Float Float|
                      Jumping Point Velocity Float | --Jerk is implicit, accel does not include gravity
                      Falling Point Velocity |
                      NewlyFalling Point Velocity Float deriving (Show)

data World = World {_blocks :: BlockMap,
                    _links :: LinkMap,
                    _cCons :: CConMap,
                    _cCis :: [Int],
                    _player :: Player}

makeLenses ''World
makeLenses ''BlockVal
makeLenses ''BlockKey
makeLenses ''Player

linkedBlocks :: LinkKey -> (BlockKey,BlockKey)
linkedBlocks (Link L2R (x,y)) = (BlockKey (x,y), BlockKey (x+1,y))
linkedBlocks (Link D2U (x,y)) = (BlockKey (x,y), BlockKey (x,y+1))

newtype AEndo a m = AEndo {appAEndo :: m (a -> a)}
instance Applicative m => Monoid (AEndo a m) where
        mempty = AEndo $ pure id
        AEndo f `mappend` AEndo g = AEndo $ (.) <$> f <*> g

atMulti :: (Applicative m,Foldable f,Ord k) =>
    f k -> (Maybe v -> m (Maybe v)) -> Map k v -> m (Map k v)
atMulti keys f mp = ($ mp) <$> appAEndo (foldMap (AEndo . alter) keys) where
    alter k = mapSet k <$> f (H.lookup k mp)

mapSet :: Ord k => k -> Maybe v -> Map k v -> Map k v
mapSet k (Just v) = H.insert k v
mapSet k Nothing = H.delete k

possibleLinks :: BlockKey -> Map Direction (BlockKey,LinkKey)
possibleLinks (BlockKey (x,y)) = H.fromList
                                 [(RtDir, (BlockKey (x+1,y),Link L2R (x  ,y  ))),
                                  (LfDir, (BlockKey (x-1,y),Link L2R (x-1,y  ))),
                                  (DnDir, (BlockKey (x,y-1),Link D2U (x  ,y-1))),
                                  (UpDir, (BlockKey (x,y+1),Link D2U (x  ,y  )))]

playerVel :: Functor f => (Velocity -> f Velocity) -> PlayerMovement -> f PlayerMovement
playerVel f (Standing support xOffset vx ax) =
    (\ (vx',_) -> Standing support xOffset vx' ax) <$> f (vx,0)
playerVel f (Jumping pt v ay) = (\ v' -> Jumping pt v' ay) <$> f v
playerVel f (Falling pt v) = (\ v' -> Falling pt v') <$> f v
playerVel f (NewlyFalling pt v t) = (\ v' -> NewlyFalling pt v' t) <$> f v

playerLoc :: Getter PlayerMovement Point
playerLoc = to _playerLoc
_playerLoc :: PlayerMovement -> Point
_playerLoc (Standing (BlockKey (x,y)) xOffset _ _) =(fromIntegral x+xOffset,fromIntegral y+(1+playerHeight)/2)
_playerLoc (Jumping pt _ _) = pt
_playerLoc (Falling pt _) = pt
_playerLoc (NewlyFalling pt _ _) = pt

playerWidth :: Float
playerWidth = 0.4
playerHeight :: Float
playerHeight = 0.8

playerTrajectory :: PlayerMovement -> Trajectory
playerTrajectory mov@(Standing (BlockKey (x,y)) xOffset vx ax) =
        RunTrajectory (mov^.playerLoc) vx ax vRunMax
playerTrajectory (Jumping (x,y) (vx,vy) ay) =
    JumpTrajectory (x,y) (vx,vy) ay g jumpJerk
playerTrajectory (Falling (x,y) (vx,vy)) =
    Parabola (x,y) (vx,vy) g
playerTrajectory (NewlyFalling (x,y) (vx,vy) timeleft) =
    Parabola (x,y) (vx,vy) g
