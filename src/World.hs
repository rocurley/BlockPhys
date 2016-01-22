{-# LANGUAGE TemplateHaskell #-}

module World where

import Graphics.Gloss.Interface.Pure.Game

import qualified Data.Map as H hiding (Map)
import Data.Map (Map)
import Control.Lens
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Debug.Trace

import Math.Polynomial

import qualified Map2D as Map2D hiding (Map2D)
import Map2D (Map2D)

scaleFactor :: Float
scaleFactor = 80
blockSize :: Float
blockSize = 0.95

g :: Float
g = -1

jumpJerk, aJump0, vJump :: Float
(jumpJerk, aJump0, vJump) = traceShowId $ let
    jumpMaxHeight = 2.2
    jumpMinHeight = 0.2
    jumpMaxTime   = 0.6
    vJump' = sqrt (-2 * jumpMinHeight * g)
    aJump0' = 2 * (-jumpMaxTime * g - 3 * vJump' +
      sqrt (g * (-18 * jumpMaxHeight + jumpMaxTime * (jumpMaxTime * g + 6 *vJump'))))/(3 * jumpMaxTime)
    jumpJerk' = -aJump0' / jumpMaxTime
    --jumpJerk = 6 * (sqrt (-2 * jumpMinHeight * g) * jumpMaxTime - 2 * jumpMaxHeight)/jumpMaxTime^3
    --aJump = 6 * jumpMaxHeight / jumpMaxTime^2 - g - 4 * sqrt (-2 * g * jumpMinHeight) /jumpMaxTime
    --vJump = sqrt (-2 * jumpMinHeight * g)
    in (jumpJerk', aJump0', vJump')

vRunMax :: Float
vRunMax = 1

jumpGraceTime :: Float
jumpGraceTime = 1

aRun :: Float
aRun = 1

type IntPt = (Int,Int)

type Velocity = (Float,Float)

data Trajectory = PolyTrajectory (Poly Float) (Poly Float) deriving (Show,Eq)
instance Arbitrary Trajectory where
  arbitrary = PolyTrajectory <$> (poly LE <$> arbitrary) <*> (poly LE <$> arbitrary)

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

data SupPos= SupPos IntPt Float deriving (Show, Eq)

data HDir = HLeft | HRight deriving (Show, Eq, Ord)

{-
Idea:
Trajectories are too smart.
The collision checker does not need to know about state transitions
This is because state transitions can be predicted and merged into the collisions.
If the transition happens first, just redo the collision checker.
Hell, only run the collision checker untill the transition in the first place.
Given this:
Trajectories become just the information the collision checker needs: no more.
data Trajectory = PolyTrajectory (Poly Float) (Poly Float) --Probably this
PlayerMovement handles the time, runoff (this is a bit fuzzy), and input transitions.
-}

data PlayerMovement = Grounded SupPos Float (Maybe HDir)
                    | Jumping Point Velocity Float --Jerk is implicit, accel does not include gravity
                    | Falling Point Velocity
                    | NewlyFalling Point Velocity Float
                    deriving (Show)
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

playerVel :: Getter PlayerMovement Velocity
playerVel = to _playerVel where
    _playerVel (Grounded _ vx _) = (vx, 0)
    _playerVel (Jumping _ v _) = v
    _playerVel (Falling _ v) = v
    _playerVel (NewlyFalling _ v _) = v

playerLoc :: Getter PlayerMovement Point
playerLoc = to _playerLoc
_playerLoc :: PlayerMovement -> Point
_playerLoc (Grounded supPos _ _) = supPosPosition supPos
_playerLoc (Jumping pt _ _) = pt
_playerLoc (Falling pt _) = pt
_playerLoc (NewlyFalling pt _ _) = pt

playerWidth :: Float
playerWidth = 0.4
playerHeight :: Float
playerHeight = 0.8

playerTrajectory :: PlayerMovement -> Trajectory
playerTrajectory mov = let
    (x, y) = mov^.playerLoc
    (vx, vy) = mov^.playerVel
    xLowTerms = [vx, x]
    yLowTerms = [vy, y]
    (xHighTerms, yHighTerms) = case mov of
        Grounded _ _ (Just HLeft)
            |vx == -vRunMax -> ([],[])
            |otherwise ->  ([-aRun], [])
        Grounded _ _ (Just HRight)
            |vx == vRunMax -> ([],[])
            |otherwise ->  ([aRun], [])
        Grounded _ _  Nothing -> case compare vx 0 of
                                  GT -> ([-aRun/2], [])
                                  EQ -> ([], [])
                                  LT -> ([ aRun/2], [])
        Jumping _ _ aJump -> ([], [jumpJerk/6, (aJump + g)/2])
        Falling _ _ -> ([],[g])
        NewlyFalling _ _ _ -> ([],[g])
    in PolyTrajectory (poly BE $ xHighTerms ++ xLowTerms) (poly BE $ yHighTerms ++ yLowTerms)

supPosPosition :: SupPos -> Point
supPosPosition (SupPos (x, y) xOffset) = (fromIntegral x + xOffset, fromIntegral y + (1+playerHeight)/2)
