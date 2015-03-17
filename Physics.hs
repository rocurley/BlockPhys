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
, atT
, predictCollision) where

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

import Data.List hiding (foldr,foldl,foldl',minimum,maximum)
import Control.Monad hiding (mapM,mapM_)
import Control.Monad.Reader hiding (mapM,mapM_)
import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Ord
import Safe hiding (at)

import Control.Lens

import Debug.Trace

import World

--Thanks -Wall.
(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

ident :: (Foreign.Storable.Storable a, Num a) => Int -> LA.Matrix a
ident n = (n><n) $ cycle $ 1: replicate n 0

linkColBuilder :: LinkKey -> H.Map BlockKey Int -> Int -> (Double, Double, Double)
--Given a link, the block map, and a given block index, gives as a tuple
--the force that will be applied to that block.
linkColBuilder linkKey @(Link L2R _) blockMap n
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
        li = H.findWithDefault (-1) l blockMap
        ri = H.findWithDefault (-1) r blockMap
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
        di = H.findWithDefault (-1) d blockMap
        ui = H.findWithDefault (-1) u blockMap

addLinkCols :: Int -> H.Map BlockKey Int -> LinkKey -> [V.Vector Double] -> [V.Vector Double]
addLinkCols nBlocks blockMap link linkCols =
    V.buildVector (3*nBlocks) ((\ (a,_,_) -> a) . linkColBuilder link blockMap) :
    V.buildVector (3*nBlocks) ((\ (_,b,_) -> b) . linkColBuilder link blockMap) :
    V.buildVector (3*nBlocks) ((\ (_,_,c) -> c) . linkColBuilder link blockMap) :
    linkCols

expandForcesList :: [Double] -> Maybe (Force, [Double])
expandForcesList (u:r:cc:rest) = Just (Force u r cc,rest)
expandForcesList _ = Nothing

solveForces :: H.Map BlockKey Force -> [LinkKey] -> H.Map LinkKey Force
solveForces externalForces activeLinks = let
    nBlocks    = H.size externalForces :: Int
    nLinks     = length activeLinks
    blocksList = H.keys externalForces :: [BlockKey]
    blockMap   = H.fromList $ zip blocksList [0..] :: H.Map BlockKey Int
    orderedForces = map (externalForces H.!)  blocksList
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
data Collision  = Collision Point Time BlockKey Direction deriving (Show,Eq,Ord)

blockStress :: BlockKey -> Reader World Stress
blockStress key = do
    maybeLinkVals <- H.toList <$> traverse (\ k -> view (links.at k)) (snd <$> possibleLinks key)
    return $ stressFromLinks [(dir,linkVal)|(dir,Just linkVal) <- maybeLinkVals]

startPoint :: Trajectory -> Point
startPoint (Parabola pt _ _) = pt
startPoint (RunTrajectory pt _ _ _) = pt
startPoint (JumpTrajectory pt _ _ _ _) = pt

startVelocity :: Trajectory -> Velocity
startVelocity (Parabola _ vel _) = vel
startVelocity (RunTrajectory _ vx _ _) = (vx,0)
startVelocity (JumpTrajectory _ vel _ _ _) = vel

naiveAtT :: Trajectory -> Time -> Trajectory
naiveAtT (Parabola (x,y) (vx,vy) ay) t = let
    (x',y') = (vx*t+x,1/2*ay*t^2 + vy*t + y)
    (vx',vy') = (vx,vy+ay*t)
    in Parabola (x',y') (vx',vy') ay
naiveAtT (JumpTrajectory (x,y) (vx,vy) aJump aG jerk) t = let
    (x',y') = (vx*t+x,1/6*jerk*t^3 + 1/2*(aG+aJump)*t^2 + vy*t + y)
    (vx',vy') = (vx,1/2*jerk*t^2 + (aG+aJump)*t + vy)
    aJump' = aJump + t*jerk
    in JumpTrajectory (x',y') (vx',vy') aJump' aG jerk
naiveAtT (RunTrajectory (x,y) vx ax vmax) t = let
        (x',y') = (1/2*ax*t^2+vx*t+x,y)
        vx' = ax*t+vx
        in RunTrajectory (x',y) vx' ax vmax

atT :: Trajectory -> Time -> Trajectory
atT trajectory@(Parabola{}) t = naiveAtT trajectory t 
atT trajectory@(RunTrajectory _ vx ax vmax) t = let
    signedVmax = vmax*signum ax 
    tMaxSpeed = (vmax -vx)/ax
    in case (compare t tMaxSpeed) of
        GT -> let
            RunTrajectory pt vx' _ vmax' = naiveAtT trajectory tMaxSpeed
            in naiveAtT (RunTrajectory pt vx' 0 vmax') (t-tMaxSpeed)
        EQ -> let 
            RunTrajectory pt vx' _ vmax' = naiveAtT trajectory tMaxSpeed
            in RunTrajectory pt vx' 0 vmax'
        LT -> naiveAtT trajectory t

atT trajectory@(JumpTrajectory _ _ aJump _ jerk) t = let
    tJumpEnd = -aJump/jerk
    in case (compare t tJumpEnd) of
        GT -> let
            JumpTrajectory pt v _ aG _ = naiveAtT trajectory tJumpEnd
            in atT (Parabola pt v aG) (t-tJumpEnd)
        EQ -> let 
            JumpTrajectory pt v _ aG _ = naiveAtT trajectory tJumpEnd
            in Parabola pt v aG
        LT -> naiveAtT trajectory t


xint :: Float -> Trajectory -> [(Point,Time)]
xint lineY trajectory@(Parabola (x,y) (vx,vy) ay) =
    -- 0 = 1/2*ay*t^2 + vy*t + (y-lineY)
    [(startPoint $ atT trajectory t,t)|t <- solveQuadratic (1/2*ay) vy (y-lineY)]
xint lineY (RunTrajectory{}) = []
xint lineY (JumpTrajectory (x,y) (vx,vy) aJump aG 0) =
    xint lineY $ Parabola (x,y) (vx,vy) (aJump+aG)
xint lineY trajectory@(JumpTrajectory (x,y) (vx,vy) aJump aG jerk) = let
    -- 0 = 1/6*jerk*t^3+1/2*(aJump+aG)*t^2 + vy*t + (y-lineY)
    tJumpEnd = -aJump/jerk
    collisionsDuringJump = [(startPoint $ atT trajectory  t,t)|
        t <- solveCubic (1/6*jerk) (1/2*(aJump+aG)) vy (y-lineY), t<tJumpEnd]
    postJumpTrajectory = atT trajectory tJumpEnd
    collisionsAfterJump = [(pt,t+tJumpEnd)|(pt,t) <- xint lineY postJumpTrajectory]
    in collisionsDuringJump++collisionsAfterJump

yint :: Float -> Trajectory -> [(Point,Time)]
yint lineX trajectory@(RunTrajectory (x,y) vx ax vmax) = let
    signedVmax = vmax*signum ax 
    tMaxSpeed = (vmax -vx)/ax
    tsPreMax = filter (< tMaxSpeed) $ solveQuadratic ax vx (x-lineX)
    RunTrajectory (x',_) vx' _ _ = atT trajectory tMaxSpeed
    tPostMax = tMaxSpeed + (lineX-x')/vx'
    ts = if tPostMax >= tMaxSpeed
         then tPostMax:tsPreMax
         else tsPreMax
    in [(startPoint $ atT trajectory t, t)|t<-ts]
yint lineX trajectory = let
    t = case trajectory of
        Parabola (x,_) (vx,_) _ -> (lineX-x)/vx
        JumpTrajectory (x,_) (vx,_) _ _ _ -> (lineX-x)/vx
    in [(startPoint $ atT trajectory t, t)]

criticalPoints :: Trajectory -> [Time]
criticalPoints (Parabola _ (_,vy) ay) =[-vy/ay]
criticalPoints (RunTrajectory _ vx ax _) = [-vx/ax]
criticalPoints trajectory@(JumpTrajectory _ (_,vy) aJump aG jerk) = let
    tJumpEnd = -aJump/jerk
    --1/2*jerk*t^2 + (aG+aJump)*t + vy =0
    criticalPointsDuringJump = filter (<tJumpEnd) $ solveQuadratic (1/2*jerk) (aG+aJump) vy
    postJumpTrajectory = atT trajectory tJumpEnd
    criticalPointsAfterJump = [t+tJumpEnd|t<- criticalPoints postJumpTrajectory]
    in criticalPointsDuringJump ++ criticalPointsAfterJump

trajectoryBox :: Trajectory -> Time -> ((Float,Float),(Float,Float))
trajectoryBox trajectory dt = let
    ts = 0:dt: filter (<dt) (criticalPoints trajectory)
    (xs,ys) = unzip $ map (startPoint . atT trajectory) ts
    in ((minimum xs,maximum xs),(minimum ys,maximum ys))

predictCollision :: Trajectory -> Float -> Reader World (Maybe Collision)
predictCollision trajectory dt = do
    let ((xmin,xmax),(ymin,ymax)) = trajectoryBox trajectory dt
    let (xTouchDist,yTouchDist) = ((playerWidth+1)/2,(playerHeight+1)/2)
    let blockCandidates = [BlockKey (blockX,blockY)|
            blockX<-[ceiling (xmin-xTouchDist)..floor (xmax+xTouchDist)],
            blockY<-[ceiling (ymin-yTouchDist)..floor (ymax+yTouchDist)]]
    blocksInBox <- filterM (\ block -> view $ blocks.to (H.member block)) blockCandidates
    --There's a potential issue where after being stopped by a collision, attempting to
    --move in the same direction will not trigger a collision. If this comes up, it can
    --probably be fixed by adding new collision surfaces slightly within the block,
    --and reporting a collision time that produces the desired result.

    --Annother option, slightly more principled: Improve the root finding algorithm such
    --that the root is as exact as floats permit while still being on the "right side".
    --This would probably prevent 
    let collisions = do --List monad
            block@(BlockKey (xBlockInt,yBlockInt)) <- blocksInBox
            let (xBlock,yBlock) = (fromIntegral xBlockInt,fromIntegral yBlockInt)
            (potentialCollisions,collisionChecker,direction) <- [
                (xint (yBlock + yTouchDist) trajectory,
                    \ collisionX _ -> abs(xBlock-collisionX) < xTouchDist, UpDir),
                (xint (yBlock - yTouchDist) trajectory,
                    \ collisionX _ -> abs(xBlock-collisionX) < xTouchDist, DnDir),
                (yint (xBlock + xTouchDist) trajectory,
                    \ _ collisionY -> abs(yBlock-collisionY) < yTouchDist, RtDir),
                (yint (xBlock - xTouchDist) trajectory,
                    \ _ collisionY -> abs(yBlock-collisionY) < yTouchDist, LfDir)]
            ((collisionX,collisionY),collisionT) <- potentialCollisions
            traceShow ((collisionX,collisionY),collisionT) $
                traceShow (atT trajectory collisionT) $
                if collisionChecker collisionX collisionY && collisionT > 0 && collisionT <dt
                then [Collision (collisionX,collisionY) collisionT block direction]
                else []
    return $ minimumByMay (comparing (\ (Collision _ t _ _) -> t)) collisions

--Passing the player as an argument instead of from the world makes
--it much easier to define recursively.
timeEvolvePlayer :: Time -> PlayerMovement -> Reader World PlayerMovement
timeEvolvePlayer t mov@(Standing (BlockKey (xInt,yInt)) xOffset vx ax) = do
    let (x,y) = (fromIntegral xInt,fromIntegral yInt)
    let trajectory = playerTrajectory mov
    let runOffTime = minimum $ filter (>= 0) $ map snd $
            xint (x+(1+playerWidth)/2) trajectory ++ xint (x-(1+playerWidth)/2) trajectory
    collision <- predictCollision trajectory $ min t runOffTime
    case collision of
        Nothing -> do --Didn't run into anything
            let patchTime = min t runOffTime 
            let newTrajectory@(RunTrajectory (x',y') vx' ax' _) =
                    atT trajectory patchTime
            let newXInt = round x'
            let newBlockKey = BlockKey (newXInt,y)
            newBlockVal <- view $ blocks.at newBlockKey
            case (compare t runOffTime,newBlockVal) of
                (GT,Just _) -> --Ran onto a new block
                    timeEvolvePlayer (t-runOffTime) $
                        Standing newBlockKey (x'- fromIntegral newXInt) vx' ax'
                (GT,Nothing) -> --Ran off into space
                    timeEvolvePlayer (t-runOffTime) $
                        Falling (x',y') (vx',0)
                (_,_) -> --Still on the first block
                    return $ Standing newBlockKey (x'- fromIntegral newXInt) vx' ax'
        Just (Collision (x',_) _ _ _) -> --Ran into something
            return $ Standing (BlockKey (xInt,yInt)) (x'-x) vx ax
timeEvolvePlayer t mov = do
    let trajectory = playerTrajectory mov
    collision <- predictCollision trajectory t
    case collision of
        Nothing -> return $ case atT trajectory t of
            Parabola pt vel _ -> case mov of
                NewlyFalling _ _ jumpTimeLeft -> case compare jumpTimeLeft t of
                    GT -> NewlyFalling pt vel (jumpTimeLeft - t)
                    _  -> Falling pt vel
                _ -> Falling pt vel
            JumpTrajectory pt vel aJump _ _ -> Jumping pt vel aJump
        Just (Collision pt tCollide blockKey@(BlockKey (blockX,blockY)) direction) -> let
            trajectoryAtCollision = atT trajectory tCollide
            vel = startVelocity trajectoryAtCollision
            in timeEvolvePlayer (t-tCollide) $ case direction of
                --No more jumping if you hit your head
                DnDir -> Falling pt (fst $ vel,0)
                UpDir -> Standing blockKey (fst pt - fromIntegral blockX) (fst vel) 0
                _ -> case mov of
                    Falling{} -> Falling pt vel
                    NewlyFalling _ _ jumpTimeLeft -> let
                        newJumpTimeLeft = jumpTimeLeft-tCollide
                        in
                            if newJumpTimeLeft > 0
                            then NewlyFalling pt vel newJumpTimeLeft
                            else Falling pt vel
                    Jumping _ _ _ -> let
                        JumpTrajectory _ _ aJump _ _ = trajectoryAtCollision
                        in Jumping pt (0,snd vel) aJump 


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

solveQuadratic :: Float -> Float -> Float -> [Float]
solveQuadratic 0 0 _ = [] --This ignores the 0=0 case...
solveQuadratic 0 b c =  [-c/b]
solveQuadratic a b c = let
    discriminant = b^2-4*a*c
    in case compare discriminant 0 of
        LT -> []
        EQ -> [-b/(2*a)]
        GT -> do
            pm <- [(+),(-)]
            return $ ((-b) `pm` sqrt discriminant)/(2*a)

solveDCubic :: Float -> Float -> [Float]
solveDCubic p q = let
    y x = x^3 + p*x+q
    xLeft = 2 * (-max 0 (-p) **(1/2) - max 0 q **(1/3))
    xRight = 2 * (max 0 (-p) **(1/2) + max 0 (-q) **(1/3))
    r = bisect y xLeft xRight
    in r:filter (/=r) (solveQuadratic 1 r (r^2+p))

solveCubic :: Float -> Float -> Float -> Float -> [Float]
solveCubic 0 b c d = solveQuadratic b c d
solveCubic a b c d = let
    p = (3*a*c-b^2)/(3*a^2)
    q = (2*b^3-9*a*b*c+27*a^2*d)/(27*a^3)
    in map (subtract (b/(3*a))) $ solveDCubic p q
