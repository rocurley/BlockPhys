{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Physics where

import Prelude hiding (foldr,foldl,mapM,mapM_,sequence,minimum,maximum)

import Graphics.Gloss.Interface.Pure.Game

import Numeric.Minimization.QuadProgPP
import qualified Data.Packed.Matrix as M
import qualified Data.Map as H
import Data.Packed.Matrix ((><))
import qualified Data.Packed.Vector as V
import qualified Numeric.LinearAlgebra.HMatrix as LA
import Numeric.LinearAlgebra.Static as SLA
import qualified Foreign.Storable (Storable)

import Data.Maybe
import Data.List hiding (foldr,foldl,foldl',minimum,maximum)
import Control.Monad hiding (mapM,mapM_)
import Control.Monad.Reader hiding (mapM,mapM_)
import Data.Foldable
import Data.Ord

import Math.Polynomial hiding (x)

import Safe hiding (at)

import Control.Lens

import Control.Arrow

import World
import PolynomialRoot
import Map2D (Map2D)
import qualified Map2D as Map2D hiding (Map2D)

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

polyCoeff :: (Eq a, Num a) => Int -> Poly a -> a
polyCoeff i p = atDef 0 (polyCoeffs LE p) i

startPoint :: Trajectory -> Point
startPoint (PolyTrajectory px py) = (polyCoeff 0 px, polyCoeff 0 py)

startVelocity :: Trajectory -> Velocity
startVelocity (PolyTrajectory px py) = (polyCoeff 1 px, polyCoeff 1 py)

atT :: Time -> Trajectory -> Trajectory
atT t (PolyTrajectory px py) = PolyTrajectory (composePoly px $ poly BE [1,t]) (composePoly py $ poly BE [1,t])

jumpEndTime :: Float -> Float -> Maybe Time
jumpEndTime _ 0 = Nothing
jumpEndTime aJump jerk = case compare (aJump*jerk) 0 of
                           EQ -> Just 0
                           GT -> Nothing
                           LT -> Just $ -aJump/jerk

xint :: Float -> Trajectory -> [(Point,Time)]
xint lineY trajectory@(PolyTrajectory _ py) = let
  p = addPoly py $ constPoly (-lineY)
  in [(startPoint $ atT t trajectory,t)|t <- findRoots p]

yint :: Float -> Trajectory -> [(Point,Time)]
yint lineX trajectory@(PolyTrajectory px _) = let
  p = addPoly px $ constPoly (-lineX)
  in [(startPoint $ atT t trajectory,t)|t <- findRoots p]

--Note that this doesn't get critical points in the past.
criticalPoints :: Trajectory -> [Time]
criticalPoints (PolyTrajectory px py) = filter (>0) $ findRoots (polyDeriv px) ++ findRoots (polyDeriv py)

trajectoryBox :: Trajectory -> Time -> ((Float,Float),(Float,Float))
trajectoryBox trajectory dt = let
    ts = 0:dt: filter (<dt) (criticalPoints trajectory)
    (xs,ys) = unzip $ map (startPoint . flip atT trajectory) ts
    in ((minimum xs,maximum xs),(minimum ys,maximum ys))

predictStaticCollision :: Time -> (Shape,Trajectory) -> Reader World (Maybe Collision)
predictStaticCollision dt (Rectangle width height, trajectory) = do
    let ((xmin,xmax),(ymin,ymax)) = trajectoryBox trajectory dt
    let (xTouchDist,yTouchDist) = ((width+1)/2,(height+1)/2)
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
  block <- view $ blocks.at (step x)
  case block of
    Nothing -> return x
    Just _  -> walkBlocks step $ step x

afterCollision :: Collision -> Movement -> Reader World (Time, Movement)
afterCollision (Collision _ t _ dir) mov = let
    trajectory = movTrajectory mov
    newTrajectory = atT t trajectory
    pt = startPoint newTrajectory
    (vx, vy) = startVelocity newTrajectory
    in case (dir,mov) of
         (DnDir, _) -> return (t, Falling pt (vx, 0))
         (UpDir, _) -> do
            support <- fromMaybe (error "No support yet hit top surface") <$> checkSupport pt
            runDir <- inputRunDirection
            return (t, Grounded support vx runDir)
         (_, _) -> (t,) <$> killVx dir <$> absorbTrajectory t newTrajectory mov

nonCollisionTransition :: Movement -> Reader World (Maybe (Time, Movement))
--Here we assume the starting state is valid.
nonCollisionTransition mov@(Grounded _ 0 Nothing) = return Nothing
nonCollisionTransition mov@(Grounded (SupPos (x,y) _) vx dir) = do
  let tChange = case dir of
          Nothing -> Just $ abs vx/aRun
          Just (HLeft)
              | vx == -vRunMax -> Nothing
              | otherwise -> Just $ (vx + vRunMax) / aRun
          Just (HRight)
              | vx == vRunMax -> Nothing
              | otherwise -> Just $ (vRunMax - vx) / aRun
      trajectory = movTrajectory mov
  (leftBound , _) <- walkBlocks (first (subtract 1)) (x,y)
  (rightBound, _) <- walkBlocks (first (+1)        ) (x,y)
  let leftRunoffs  = yint (fromIntegral leftBound  - (1 + playerWidth)/2) trajectory
      rightRunoffs = yint (fromIntegral rightBound + (1 + playerWidth)/2) trajectory
      firstRunoff = minimumMay $ filter (>=0) $ map snd $ leftRunoffs ++ rightRunoffs
      handleRunoff t = let
        newTrajectory = atT t trajectory
        in return $ Just (t, NewlyFalling (startPoint newTrajectory) (startVelocity newTrajectory) jumpGraceTime)
      handleTrajectoryChange t = do
          let newTrajectory = atT t trajectory
          newSupPos <- fromMaybe (error "Support expected but missing")
                    <$> checkSupport (startPoint newTrajectory)
          return $ case dir of
                     Nothing -> Just $ (t, Grounded newSupPos 0 Nothing)
                     Just (HLeft) -> Just $ (t, Grounded newSupPos (-vRunMax) $ Just HLeft)
                     Just (HRight) -> Just $ (t, Grounded newSupPos vRunMax $ Just HRight)
  case (firstRunoff,tChange) of
      (Just tr, Just tc)
          |tr < tc -> handleRunoff tr
          |otherwise -> handleTrajectoryChange tc
      (Just tr, Nothing) -> handleRunoff tr
      (Nothing, Just tc) -> handleTrajectoryChange tc
      (Nothing, Nothing) -> return Nothing


nonCollisionTransition Falling{} = return Nothing
nonCollisionTransition mov@(Jumping _ _ aJump) = return $ do
    t <- jumpEndTime aJump jumpJerk
    let trajectory = movTrajectory mov
        newTrajectory = atT t trajectory
    return $ (t, Falling (startPoint newTrajectory) (startVelocity newTrajectory))
nonCollisionTransition mov@(NewlyFalling _ _ t) = return $ Just $ let
    trajectory = movTrajectory mov
    newTrajectory = atT t trajectory
    in (t, Falling (startPoint newTrajectory) (startVelocity newTrajectory))

nextTransition :: Time -> Shape -> Movement -> Reader World (Maybe (Time, Movement))
nextTransition t shape mov = do
  maybeCollision <- predictStaticCollision t (shape, movTrajectory mov)
  maybeCTransition <- case maybeCollision of --lift is annoying
                        Nothing -> return Nothing
                        Just collision -> Just <$> afterCollision collision mov
  maybeNCTransition <- nonCollisionTransition mov
  return $ minimumByMay (comparing fst) $ filter ((<=t) . fst) $ catMaybes [maybeNCTransition, maybeCTransition]

killVx :: Direction -> Movement -> Movement
killVx LfDir (Grounded support _ (Just HRight)) = Grounded support 0 Nothing
killVx RtDir (Grounded support _ (Just HLeft)) = Grounded support 0 Nothing
killVx _ (Grounded support _ dir) = Grounded support 0 dir
killVx _ (Falling pt (_, vy)) = Falling pt (0, vy)
killVx _ (NewlyFalling pt (_, vy) t) = NewlyFalling pt (0, vy) t
killVx _ (Jumping pt (_, vy) aJump) = Jumping pt (0, vy) aJump

absorbTrajectory :: Time -> Trajectory -> Movement -> Reader World Movement
absorbTrajectory t trajectory mov = do
    let pt = startPoint trajectory
        (vx, vy) = startVelocity trajectory
    maybeSupport <- checkSupport pt
    let support = flip fromMaybe maybeSupport $ error $
          "Attempted to absorb trajectory after running off edge"
            ++ "\nTime: " ++ show t
            ++ "\nTrajectory: " ++ show trajectory
            ++ "\nMovement: " ++ show mov
    return $ case mov of
             Falling{} -> Falling pt (vx, vy)
             NewlyFalling _ _ tJump
               | tJump < t -> error "Attempted to absorb trajectory after jump end"
               | otherwise -> NewlyFalling pt (vx, vy) (tJump - t)
             Jumping _ _ aJump -> Jumping pt (vx, vy) (aJump + t * jumpJerk)
             Grounded _ _ dir -> Grounded support vx dir

checkSupport :: Point -> Reader World (Maybe SupPos)
checkSupport (x0, y0) = do
    let y = round $ y0 - (1 + playerHeight)/2 -- This is a bit dubious
        xLeft = floor x0
        xRight = ceiling x0
        leftOffset = x0 - fromIntegral xLeft
        rightOffset = x0 - fromIntegral xRight
        xLeftInRange  =  leftOffset  < (1 + playerWidth)/2
        xRightInRange = -rightOffset < (1 + playerWidth)/2
    xLeftExists  <- isJust <$> view (blocks.at (xLeft , y))
    xRightExists <- isJust <$> view (blocks.at (xRight, y))
    return $ case (xLeftInRange && xLeftExists, xRightInRange && xRightExists) of
        (False, False) -> Nothing
        (True , False) -> Just $ SupPos (xLeft , y) leftOffset
        (False, True ) -> Just $ SupPos (xRight, y) rightOffset
        (True , True )
          | leftOffset < -rightOffset -> Just $ SupPos (xLeft , y) leftOffset
          | otherwise -> Just $ SupPos (xRight, y) rightOffset

--Passing the player as an argument instead of from the world makes
--it much easier to define recursively.
timeEvolveMovement :: Time -> Shape -> Movement -> Reader World Movement
timeEvolveMovement t shape mov = do
    movChecked <- case mov of
              Grounded support vx dir -> do
                let pt = supPosPosition support
                support' <- checkSupport pt
                return $ maybe (NewlyFalling pt (vx, 0) jumpGraceTime) (\s -> Grounded s vx dir) support'
              _ -> return mov
    transition <- nextTransition t shape movChecked
    case transition of
      Nothing -> absorbTrajectory t (atT t $ movTrajectory movChecked) movChecked
      Just (t', mov') -> timeEvolveMovement (t-t') shape mov'

predictDynamicCollision :: (Shape, Trajectory) -> (Shape, Trajectory) -> Maybe Time
predictDynamicCollision (Rectangle w1 h1, t1) (Rectangle w2 h2, t2) = let
  xTouchDist = (w1 + w2)/2
  yTouchDist = (h1 + h2)/2
  diffTrajectory = trajectoryDiff t1 t2
  collisions = join
      [ [t | ((x,y),t) <- yint    xTouchDist diffTrajectory, abs y <= yTouchDist]
      , [t | ((x,y),t) <- yint (-xTouchDist) diffTrajectory, abs y <= yTouchDist]
      , [t | ((x,y),t) <- xint    yTouchDist diffTrajectory, abs x <= xTouchDist]
      , [t | ((x,y),t) <- xint (-yTouchDist) diffTrajectory, abs x <= xTouchDist]
      ]
  in minimumMay $ filter (>0) collisions

