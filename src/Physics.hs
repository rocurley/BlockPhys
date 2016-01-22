{-# LANGUAGE DataKinds #-}

module Physics
( solveForces
, fg
, Stress(..)
, Time
, stressFromLinks
, blockStress
, Trajectory(..)
, Collision(..)
, startPoint
, naiveAtT
, atT
, trajectoryBox
, criticalPoints
, jumpEndTime
, yint
, xint
, predictCollision
, timeEvolvePlayerMovement) where

import Prelude hiding (foldr,foldl,mapM,mapM_,sequence,minimum,maximum,(^))
import qualified Prelude ((^))

import Graphics.Gloss.Interface.Pure.Game

import Numeric.Minimization.QuadProgPP
import qualified Data.Packed.Matrix as M
import qualified Data.Map as H
import Data.Packed.Matrix ((><))
import qualified Data.Packed.Vector as V
import qualified Numeric.LinearAlgebra.HMatrix as LA
import Numeric.LinearAlgebra.Static as SLA
import qualified Foreign.Storable (Storable)
import Numeric.IEEE

import Data.Maybe
import Data.List hiding (foldr,foldl,foldl',minimum,maximum)
import Control.Monad hiding (mapM,mapM_)
import Control.Monad.Reader hiding (mapM,mapM_)
import Data.Foldable
import Data.Ord

import Math.Polynomial

import Safe hiding (at)
import qualified Safe

import Control.Lens

import World
import PolynomialRoot
import Map2D (Map2D)
import qualified Map2D as Map2D hiding (Map2D)

--Thanks -Wall.
(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

ident :: (Foreign.Storable.Storable a, Num a) => Int -> LA.Matrix a
ident n = (n><n) $ cycle $ 1: replicate n 0

linkColBuilder :: LinkKey -> Map2D Int Int Int -> Int -> (Double, Double, Double)
--Given a link, the block map, and a given block index, gives as a tuple
--the force that will be applied to that block.
linkColBuilder linkKey@(Link L2R _) blockMap n
    --force type     u  r cc
    | n == 3*li   =( 1, 0, 0)--up     -|
    | n == 3*li+1 =( 0, 1, 0)--right  -|- affect on left block
    | n == 3*li+2 =( 1, 0, 1)--rotCCW -|
    | n == 3*ri   =(-1, 0, 0)-- |
    | n == 3*ri+1 =( 0,-1, 0)-- |-------- affect on right block
    | n == 3*ri+2 =( 1, 0,-1)-- |
    | otherwise   =( 0, 0, 0)
    where
        (l,r) = linkedBlocks linkKey
        li = Map2D.findWithDefault (-1) l blockMap
        ri = Map2D.findWithDefault (-1) r blockMap
linkColBuilder linkKey @(Link D2U _) blockMap n
    --force type     u  r cc
    | n == 3*di   =( 1, 0, 0)--up     -|
    | n == 3*di+1 =( 0, 1, 0)--right  -|- affect on bottom block
    | n == 3*di+2 =( 0, 1, 1)--rotCCW -|
    | n == 3*ui   =(-1, 0, 0)-- |
    | n == 3*ui+1 =( 0,-1, 0)-- |-------- affect on top block
    | n == 3*ui+2 =( 0, 1,-1)-- |
    | otherwise   =( 0, 0, 0)
    where
        (d,u) = linkedBlocks linkKey
        di = Map2D.findWithDefault (-1) d blockMap
        ui = Map2D.findWithDefault (-1) u blockMap

addLinkCols :: Int -> Map2D Int Int Int -> LinkKey -> [V.Vector Double] -> [V.Vector Double]
addLinkCols nBlocks blockMap link linkCols =
    V.buildVector (3*nBlocks) ((\ (a,_,_) -> a) . linkColBuilder link blockMap) :
    V.buildVector (3*nBlocks) ((\ (_,b,_) -> b) . linkColBuilder link blockMap) :
    V.buildVector (3*nBlocks) ((\ (_,_,c) -> c) . linkColBuilder link blockMap) :
    linkCols

expandForcesList :: [Double] -> Maybe (Force, [Double])
expandForcesList (u:r:cc:rest) = Just (Force u r cc,rest)
expandForcesList _ = Nothing

solveForces :: Map2D Int Int Force -> [LinkKey] -> H.Map LinkKey Force
solveForces externalForces activeLinks = let
    nBlocks    = Map2D.size externalForces :: Int
    nLinks     = length activeLinks
    blocksList = Map2D.keys externalForces :: [IntPt]
    blockMap   = Map2D.fromList $ zip blocksList [0..] :: Map2D Int Int Int
    orderedForces = map (externalForces Map2D.!)  blocksList
    a = ident $ 3*nLinks
    b = V.fromList $ replicate (3*nLinks) 0
    c = M.fromColumns $ foldr (addLinkCols nBlocks blockMap) [] activeLinks
    --A fold would do this without list concatenation
    d = V.fromList $ join [[up, right, rotRight]|Force up right rotRight <- orderedForces]
    (forcesVector, energy) = case solveQuadProg (a,b) (Just (c,d)) Nothing of
        Right (f,e) -> (f,e)
        Left err -> error $ show err
    in H.fromList $ zip activeLinks $ unfoldr expandForcesList (V.toList forcesVector)

fg :: Force
fg = Force (-1) 0 0

newtype Stress = Stress (L 2 2) deriving (Show)
instance Monoid Stress where
    mempty = Stress $ matrix [0,0,0,0]
    mappend (Stress a) (Stress b)= Stress $ a+b

-- |dFx/dx,dFx/dy|
-- |dFy/dx,dFy/dy|

stressFromLinks :: [(Direction,LinkVal)] -> Stress
stressFromLinks = foldMap toStress where
    toStress  :: (Direction, LinkVal) -> Stress
    toStress (_, OffLink) = mempty
    toStress (direction, OnLink (Force up right rotCCW)) =
        let (upMat,rightMat,rotCCWMat) = case direction of
                UpDir -> (matrix [0,0,0,1],matrix [0,1,0,0],matrix [0,1,0,0])
                DnDir -> (matrix [0,0,0,1],matrix [0,1,0,0],matrix [0,-1,0,0])
                RtDir -> (matrix [0,1,0,0],matrix [1,0,0,0],matrix [0,0,-1,0])
                LfDir -> (matrix [0,1,0,0],matrix [1,0,0,0],matrix [0,0,1,0])
            in Stress $ konst up*upMat+konst right*rightMat+konst rotCCW*rotCCWMat
type Time = Float
data Collision  = Collision Point Time IntPt Direction deriving (Show,Eq,Ord)

blockStress :: IntPt -> Reader World Stress
blockStress key = do
    maybeLinkVals <- H.toList <$> traverse (\ k -> view (links.at k)) (snd <$> possibleLinks key)
    return $ stressFromLinks [(dir,linkVal)|(dir,Just linkVal) <- maybeLinkVals]

polyCoeff :: Num a => Int -> Poly a -> a
polyCoeff i p = atDef 0 i $ polyCoeffs LE p

startPoint :: Trajectory -> Point
startPoint (PolyTrajectory px py) = (polyCoeff 0 px, polyCoeff 0 py)

startVelocity :: Trajectory -> Velocity
startVelocity (PolyTrajectory px py) = (polyCoeff 1 px, polyCoeff 1 py)

shiftTrajectory :: Time -> Trajectory -> Trajectory
shiftTrajectory t (PolyTrajectory p) = composePoly p $ poly BE [1,t]

atT :: Trajectory -> Time -> Trajectory
atT trajectory@(Parabola{}) t = naiveAtT trajectory t
atT trajectory@(RunTrajectory pt vx ax vmax) t
  | vmax < 0 = error  $ "vmax must not be negative: vmax = " ++ show vmax
  | abs vx > abs vmax = atT (RunTrajectory pt (signum vx * abs vmax) ax vmax) t
  | otherwise = let
    signedVmax = vmax*signum ax
    tMaxSpeed = case ax of
      0 -> infinity
      _ -> (signedVmax - vx)/ax
    in case (compare t tMaxSpeed) of
        GT -> let
            RunTrajectory pt vx' _ vmax' = naiveAtT trajectory tMaxSpeed
            in naiveAtT (RunTrajectory pt vx' 0 vmax') (t-tMaxSpeed)
        EQ -> let
            RunTrajectory pt vx' _ vmax' = naiveAtT trajectory tMaxSpeed
            in RunTrajectory pt vx' 0 vmax'
        LT -> naiveAtT trajectory t

atT trajectory@(JumpTrajectory _ _ aJump _ jerk) t =
  case jumpEndTime aJump jerk of
    Nothing -> naiveAtT trajectory t
    Just tJumpEnd -> case (compare t tJumpEnd) of
      GT -> let
              JumpTrajectory pt v _ aG _ = naiveAtT trajectory tJumpEnd
              in atT (Parabola pt v aG) (t-tJumpEnd)
      EQ -> let
              JumpTrajectory pt v _ aG _ = naiveAtT trajectory tJumpEnd
              in Parabola pt v aG
      LT -> naiveAtT trajectory t

jumpEndTime :: Float -> Float -> Maybe Time
jumpEndTime _ 0 = Nothing
jumpEndTime aJump jerk = case compare (aJump*jerk) 0 of
                           EQ -> Just 0
                           GT -> Nothing
                           LT -> Just $ -aJump/jerk

xint :: Float -> Trajectory -> [(Point,Time)]
xint lineY trajectory@(PolyTrajectory px py) = let
  p = addPoly py $ constPoly (-lineY)
  in [(startPoint $ atT trajectory t,t)|t <- findRoots p]

yint :: Float -> Trajectory -> [(Point,Time)]
yint lineX trajectory@(PolyTrajectory px py) = let
  p = addPoly px $ constPoly (-lineX)
  in [(startPoint $ atT trajectory t,t)|t <- findRoots p]

--Note that this doesn't get critical points in the past.
criticalPoints :: Trajectory -> [Time]
criticalPoints (PolyTrajectory p) = filter (>0) $ findRoots $ polyDeriv p

trajectoryBox :: Trajectory -> Time -> ((Float,Float),(Float,Float))
trajectoryBox trajectory dt = let
    ts = 0:dt: filter (<dt) (criticalPoints trajectory)
    (xs,ys) = unzip $ map (startPoint . atT trajectory) ts
    in ((minimum xs,maximum xs),(minimum ys,maximum ys))

predictCollision :: Trajectory -> Float -> Reader World (Maybe Collision)
predictCollision trajectory dt = do
    let ((xmin,xmax),(ymin,ymax)) = trajectoryBox trajectory dt
    let (xTouchDist,yTouchDist) = ((playerWidth+1)/2,(playerHeight+1)/2)
    blocksInBox <-
        Map2D.rangeInc
          (ceiling $ xmin - xTouchDist, ceiling $ ymin - yTouchDist)
          (floor $ xmax + xTouchDist, floor $ ymax + yTouchDist)
        <$> view blocks
    let
      collisions :: [Collision]
      collisions = do
        blockKey@(xBlockInt,yBlockInt) <- Map2D.keys blocksInBox
        let (xBlock,yBlock) = (fromIntegral xBlockInt,fromIntegral yBlockInt)
        collision@(Collision _ ct _ _) <- join [
            [Collision (cx,cy) ct blockKey UpDir |
                ((cx, cy), ct) <- xint (yBlock + yTouchDist) trajectory, abs(xBlock-cx) < xTouchDist],
            [Collision (cx,cy) ct blockKey DnDir |
                ((cx, cy), ct) <- xint (yBlock - yTouchDist) trajectory, abs(xBlock-cx) < xTouchDist],
            [Collision (cx,cy) ct blockKey RtDir |
                ((cx, cy), ct) <- yint (xBlock + xTouchDist) trajectory, abs(yBlock-cy) < yTouchDist],
            [Collision (cx,cy) ct blockKey LfDir |
                ((cx, cy), ct) <- yint (xBlock - xTouchDist) trajectory, abs(yBlock-cy) < yTouchDist]]
        guard (dt > ct && ct > 0)
        return collision
    return $ minimumByMay (comparing (\ (Collision _ t _ _) -> t)) collisions

walkBlocks :: (IntPt -> IntPt) -> IntPt -> Reader World IntPt
walkBlocks step x = do
  block <- view blocks.at (step x)
  case block of
    Nothing -> return x
    Just _  -> walkBlocks step $ step x

afterCollision :: Collision -> PlayerMovement -> Reader World (Time, PlayerMovement)
afterCollision (Collision _ t _ dir) mov = let
    trajectory = playerTrajectory mov
    newTrajectory = atT t trajectory
    pt = startPoint newTrajectory
    (vx, vy) = startVelocity newTrajectory
    in case (dir,mov) of
         (DnDir, _) -> return (t, Falling pt (vx, 0))
         (UpDir, _) -> do
            support <- fromMaybe (error "No support yet hit top surface") $ checkSupport pt
            return (t, Grounded support vx Nothing) -- That Nothing could cause problems later
         (_, _) -> killVx $ absorbTrajectory t newTrajectory mov

nonCollisionTransition :: PlayerMovement -> Reader World (Maybe (Time, PlayerMovement))
--Here we assume the starting state is valid.
nonCollisionTransition mov@(Grounded (SupPos (x,y) xOffset) 0 Nothing) = return Nothing
nonCollisionTransition mov@(Grounded (SupPos (x,y) xOffset) vx Nothing) = do
  let tStop = abs vx/aRun
      trajectory = playerTrajectory mov
  (leftBound , _) = walkBlocks (first (subtract 1)) (x,y)
  (rightBound, _) = walkBlocks (first (+1)        ) (x,y)
  leftRunoff  <- xint (fromInteger leftBound  - (1 + playerWidth)/2) trajectory
  rightRunoff <- xint (fromInteger rightBound + (1 + playerWidth)/2) trajectory
  let firstRunoff = minimumMay $ map fst $ catMaybes [leftRunoff, rightRunoff]
  case firstRunoff of
    Just tRunoff
      |tRunoff <= tStop -> let
        newTrajectory = atT tRunoff trajectory
        in return $ Just (tRunoff, Falling (startPoint newTrajectory) (startVelocity newTrajectory))
    _ -> do
      let newTrajectory = atT tStop trajectory
      newSupPos <- fromMaybe (error "Support expected but missing") $ checkSupport (startPoint newTrajectory)
      in Just <$> (tStop, Grounded newSupPos 0)
nonCollisionTransition Falling{} = return Nothing
nonCollisionTransition mov@(Jumping _ _ aJump) = return $ do
    t <- jumpEndTime aJump jumpJerk
    let trajectory = playerTrajectory mov
        newTrajectory = atT t trajectory
    return $ (t, Falling (newTrajectory^.startPoint) (newTrajectory^.startVelocity))
nonCollisionTransition mov@(NewlyFalling _ _ t) = return $ Just $ let
    let trajectory = playerTrajectory mov
        newTrajectory = atT t trajectory
    return $ (t, Falling (newTrajectory^.startPoint) (newTrajectory^.startVelocity))

nextTransition :: PlayerMovement -> Reader World (Maybe (t, PlayerMovement))
nextTransition mov = do
  maybeCollision <- predictCollision mov
  maybeCTransition <- case maybeCollision of --lift is annoying
                        Nothing -> return Nothing
                        Just collision -> Just <$> afterCollision collision mov
  maybeNCTransition <- nonCollisionTransition mov
  return $ minimumByMay fst $ catMaybes [maybeCTransition, maybeNCTransition]

killVx :: PlayerMovement -> PlayerMovement
killVx (Grounded support _ dir) = Grounded support 0 dir
killVx (Falling pt (_, vy)) = Falling pt (0, vy)
killVx (NewlyFalling pt (_, vy) t) = NewlyFalling pt (0, vy) t
killVx (Jumping pt (_, vy) aJump) = Jumping pt (0, vy) aJump

absorbTrajectory :: Time -> Trajectory -> PlayerMovement -> Reader World PlayerMovement
absorbTrajectory t trajectory mov = do
    pt = startPoint trajectory
    (vx, vy) = startVelocity trajectory
    maybeSupport <- checkSupport pt
    let support = fromMaybe (error "Attempted to absorb trajectory after running off edge") maybeSupport
    return case mov of
             Falling{} -> Falling pt (vx, vy)
             NewlyFalling _ _ tJump
               | tJump < t -> error "Attempted to absorb trajectory after jump end"
               | otherwise -> NewlyFalling pt (vx, vy) (tJump - t)
             Jumping _ _ aJump -> Jumping pt (vx, vy) (aJump - t * jumpJerk)
             Grounded _ _ dir = Grounded support vx dir

checkSupport :: Point -> Reader World (Maybe SupPos)
checkSupport (x0, y0) = do
    let y = round $ y0 - (1 + playerHeight)/2 -- This is a bit dubious
        xLeft = floor y
        xRight = ceil y
        leftOffset = x0 - fromIntegral xLeft
        rightOffset = x0 - fromIntegral xRight
        xLeftInRange  =  leftOffset  < (1 + playerWidth)/2
        xRightInRange = -rightOffset < (1 + playerWidth)/2
    xLeftExists  <- isJust <$> view (blocks.at (xLeft , y)
    xRightExists <- isJust <$> view (blocks.at (xRight, y)
    return case (xLeftInRange && xLeftExists, xRightInRange && xRightInRange) of
        (False, False) -> Nothing
        (True , False) -> Just $ SupPos (xLeft , y) leftOffset
        (False, True ) -> Just $ SupPos (xRight, y) rightOffset
        (True , True )
          | leftOffset < -rightOffset -> Just $ SupPos (xLeft , y) leftOffset
          | otherwise -> Just $ SupPos (xRight, y) rightOffset

--Passing the player as an argument instead of from the world makes
--it much easier to define recursively.
timeEvolvePlayerMovement :: Time -> PlayerMovement -> Reader World PlayerMovement
timeEvolvePlayerMovement t mov = do
    transition <- nextTransition mov
    case transition of
      Nothing -> absorbTrajectory t (atT t $ playerTrajectory mov) mov
      Just (t', mov') = timeEvolvePlayerMovement (t-t') mov'

bisect :: (Float -> Float) -> Float -> Float -> Float
bisect f xLow xHigh = let
    xLeft = min xLow xHigh
    xRight = max xLow xHigh
    in if succIEEE xLeft == xRight || xLeft == xRight
    then if abs (f xLeft) < abs (f xRight)
         then xLeft
         else xRight
    else let
        xNew = (xLow+xHigh)/2
        yNew = f xNew
        in case compare yNew 0 of
            EQ -> xNew
            GT -> bisect f xLow xNew
            LT -> bisect f xNew xHigh
