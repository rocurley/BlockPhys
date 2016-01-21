{-# LANGUAGE TemplateHaskell #-}

module World where

import Graphics.Gloss.Interface.Pure.Game

import qualified Data.Map as H hiding (Map)
import Data.Map (Map)
import Control.Lens
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Debug.Trace

import qualified Map2D as Map2D hiding (Map2D)
import Map2D (Map2D)

scaleFactor :: Float
scaleFactor = 80
blockSize :: Float
blockSize = 0.95

g :: Float
g = -1

jumpJerk, aJump, vJump :: Float
(jumpJerk, aJump, vJump) = traceShowId $ let
    jumpMaxHeight = 2.2
    jumpMinHeight = 0.2
    jumpMaxTime   = 0.6
    vJump = sqrt (-2 * jumpMinHeight * g)
    aJump = 2 * (-jumpMaxTime * g - 3 * vJump +
      sqrt (g * (-18 * jumpMaxHeight + jumpMaxTime * (jumpMaxTime * g + 6 *vJump))))/(3 * jumpMaxTime)
    jumpJerk = -aJump / jumpMaxTime
    --jumpJerk = 6 * (sqrt (-2 * jumpMinHeight * g) * jumpMaxTime - 2 * jumpMaxHeight)/jumpMaxTime^3
    --aJump = 6 * jumpMaxHeight / jumpMaxTime^2 - g - 4 * sqrt (-2 * g * jumpMinHeight) /jumpMaxTime
    --vJump = sqrt (-2 * jumpMinHeight * g)
    in (jumpJerk, aJump, vJump)

vRunMax :: Float
vRunMax = 5
jumpGraceTime :: Float
jumpGraceTime = 1

type IntPt = (Int,Int)

type Velocity = (Float,Float)

data Trajectory = Parabola Point Velocity Float |
                  RunTrajectory Point Float Float Float |
                  JumpTrajectory Point Velocity Float Float Float deriving (Show,Eq,Ord)
instance Arbitrary Trajectory where
  arbitrary = do
    n <- choose (0 :: Int ,2)
    case n of
      0 -> Parabola <$> arbitrary <*> arbitrary <*> arbitrary
      1 -> RunTrajectory <$> arbitrary <*> arbitrary <*> arbitrary <*> fmap abs arbitrary
      2 -> JumpTrajectory <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary  <*> arbitrary
      _ -> error "Out of bounds random trajectory constructor"

data BlockType = Normal | Bedrock deriving (Eq,Ord,Show)
data BlockVal = BlockVal {_blockType :: BlockType, _cci :: Int} deriving (Eq,Ord,Show)
type Block = (IntPt, BlockVal)
type BlockMap = Map2D Int Int BlockVal

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
instance Arbitrary Player where
  arbitrary = Player <$> arbitrary

data PlayerMovement = Standing IntPt Float Float Float |
                      Jumping Point Velocity Float | --Jerk is implicit, accel does not include gravity
                      Falling Point Velocity |
                      NewlyFalling Point Velocity Float deriving (Show)
instance Arbitrary PlayerMovement where
  --This is bad and I should feel bad.
  arbitrary = do
    n <- choose (0 :: Int ,2)
    case n of
      0 -> Jumping (0,0) <$> arbitrary <*> arbitrary
      1 -> Falling (0,0) <$> arbitrary
      2 -> NewlyFalling (0,0) <$> arbitrary <*> arbitrary
      _ -> error "Out of bounds random movement constructor"

data World = World {_blocks :: BlockMap,
                    _links :: LinkMap,
                    _cCons :: CConMap,
                    _cCis :: [Int],
                    _player :: Player}

makeLenses ''World
makeLenses ''BlockVal
makeLenses ''Player

linkedBlocks :: LinkKey -> (IntPt, IntPt)
linkedBlocks (Link L2R (x,y)) = ((x,y), (x+1,y))
linkedBlocks (Link D2U (x,y)) = ((x,y), (x,y+1))

possibleLinks :: IntPt -> Map Direction (IntPt, LinkKey)
possibleLinks (x,y) = H.fromList
                                 [(RtDir, ((x+1,y),Link L2R (x  ,y  ))),
                                  (LfDir, ((x-1,y),Link L2R (x-1,y  ))),
                                  (DnDir, ((x,y-1),Link D2U (x  ,y-1))),
                                  (UpDir, ((x,y+1),Link D2U (x  ,y  )))]

playerVel :: Functor f => (Velocity -> f Velocity) -> PlayerMovement -> f PlayerMovement
playerVel f (Standing support xOffset vx ax) =
    (\ (vx',_) -> Standing support xOffset vx' ax) <$> f (vx,0)
playerVel f (Jumping pt v ay) = (\ v' -> Jumping pt v' ay) <$> f v
playerVel f (Falling pt v) = (\ v' -> Falling pt v') <$> f v
playerVel f (NewlyFalling pt v t) = (\ v' -> NewlyFalling pt v' t) <$> f v

playerLoc :: Getter PlayerMovement Point
playerLoc = to _playerLoc
_playerLoc :: PlayerMovement -> Point
_playerLoc (Standing (x,y) xOffset _ _) =(fromIntegral x+xOffset,fromIntegral y+(1+playerHeight)/2)
_playerLoc (Jumping pt _ _) = pt
_playerLoc (Falling pt _) = pt
_playerLoc (NewlyFalling pt _ _) = pt

playerWidth :: Float
playerWidth = 0.4
playerHeight :: Float
playerHeight = 0.8

playerTrajectory :: PlayerMovement -> Trajectory
playerTrajectory mov@(Standing _ _ vx ax) =
    RunTrajectory (mov^.playerLoc) vx ax vRunMax
playerTrajectory (Jumping pt v ay) =
    JumpTrajectory pt v ay g jumpJerk
playerTrajectory (Falling pt v) =
    Parabola pt v g
playerTrajectory (NewlyFalling pt v timeleft) =
    Parabola pt v g

trajectoryMovement :: Trajectory -> PlayerMovement
trajectoryMovement (RunTrajectory (x, y) vx ax _) = let
  (xi, yi) = (round x, round y)
  in Standing (xi, yi) (x - fromIntegral xi) vx ax
trajectoryMovement (JumpTrajectory pt v ay _ _) =
  Jumping pt v ay
--Note that this should maybe be NewlyFalling: you need to be careful here.
trajectoryMovement (Parabola pt vel _) =
  Falling pt vel
