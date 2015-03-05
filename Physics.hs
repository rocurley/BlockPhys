{-# LANGUAGE DataKinds #-}

module Physics
( solveForces
, g
, Stress(..)
, Time
, stressFromLinks
, Trajectory(..)
, Collision(..)
, startPoint
, atT
, predictCollision) where

import Prelude hiding (foldr,foldl,mapM,mapM_,sequence,minimum,maximum,(^))
import qualified Prelude

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
import Control.Monad.State.Strict hiding (mapM,mapM_)
import Data.Monoid
import Data.Foldable
import Data.Ord
import Safe

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

g :: Force
g = Force (-1) 0 0

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

startPoint :: Trajectory -> Point
startPoint (Parabola pt _ _) = pt
startPoint (JumpTrajectory pt _ _ _ _) = pt

atT :: Trajectory -> Time -> Trajectory
atT (Parabola (x,y) (vx,vy) ay) t = let
    (x',y') = (vx*t+x,1/2*ay*t^2 + vy*t + y)
    (vx',vy') = (vx,vy+ay*t)
    in Parabola (x',y') (vx',vy') ay
atT traj@(JumpTrajectory (x,y) (vx,vy) aJump aG jerk) t = let
    tJumpEnd = -aJump/jerk
    naiveAtT t' = let
        (x',y') = (vx*t'+x,1/6*jerk*t'^3 + 1/2*(aG+aJump)*t'^2 + vy*t' + y)
        (vx',vy') = (vx,1/2*jerk*t'^2 + (aG+aJump)*t' + vy)
        aJump' = aJump + t'*jerk
        in JumpTrajectory (x',y') (vx',vy') aJump' aG jerk
    in case compare tJumpEnd t of
        GT -> let
            JumpTrajectory pt v _ aG _ = naiveAtT tJumpEnd
            in atT (Parabola pt v aG) (t-tJumpEnd)
        EQ -> let 
            JumpTrajectory pt v _ aG _ = naiveAtT tJumpEnd
            in Parabola pt v aG
        LT -> naiveAtT t


xint :: Float -> Trajectory -> [(Point,Time)]
xint lineY trajectory@(Parabola (x,y) (vx,vy) ay) =
    -- 0 = 1/2*ay*t^2 + vy*t + (y-lineY)
    [(startPoint $ atT trajectory t,t)|t <- solveQuadratic (1/2*ay) vy (y-lineY)]
xint lineY trajectory@(JumpTrajectory (x,y) (vx,vy) aJump aG jerk) = let
    -- 0 = 1/6*jerk*t^3+1/2*(aJump+aG)*t^2 + vy*t + (y-lineY)
    tJumpEnd = -aJump/jerk
    collisionsDuringJump = [(startPoint $ atT trajectory  t,t)|
        t <- solveCubic (1/6*jerk) (1/2*(aJump+aG)) vy (y-lineY), t<tJumpEnd]
    postJumpTrajectory = atT trajectory tJumpEnd
    collisionsAfterJump = [(pt,t+tJumpEnd)|(pt,t) <- xint lineY postJumpTrajectory]
    in collisionsDuringJump++collisionsAfterJump

yint :: Float -> Trajectory -> [(Point,Time)]
yint lineX trajectory = let
    t = case trajectory of
        Parabola (x,_) (vx,_) _ -> (lineX-x)/vx
        JumpTrajectory (x,_) (vx,_) _ _ _ -> (lineX-x)/vx
    in [(startPoint $ atT trajectory t, t)]

criticalPoints :: Trajectory -> [Time]
criticalPoints (Parabola _ (_,vy) ay) =[-vy/ay]
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

predictCollision :: Trajectory -> Float -> State World (Maybe Collision)
predictCollision trajectory dt = do
    let ((xmin,xmax),(ymin,ymax)) = trajectoryBox trajectory dt
    let (xTouchDist,yTouchDist) = ((playerWidth+1)/2,(playerHeight+1)/2)
    let blockCandidates = [BlockKey (blockX,blockY)|
            blockX<-[ceiling (xmin-xTouchDist)..floor (xmax+xTouchDist)],
            blockY<-[ceiling (ymin-yTouchDist)..floor (ymax+yTouchDist)]]
    blocksInBox <- filterM (\ block -> use $ blocks.to (H.member block)) blockCandidates
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
            if collisionChecker collisionX collisionY && collisionT > 0 && collisionT <dt
            then [Collision (collisionX,collisionY) collisionT block direction]
            else []
    return $ minimumByMay (comparing (\ (Collision _ t _ _) -> t)) collisions

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
    epsilon = maximum [abs xLeft, abs xRight, xRight-xLeft] * 10**(-6)
    r = bisect y xLeft xRight
    in r:filter (/=r) (solveQuadratic 1 r (r^2+p))

solveCubic :: Float -> Float -> Float -> Float -> [Float]
solveCubic 0 b c d = solveQuadratic b c d
solveCubic a b c d = let
    p = (3*a*c-b^2)/(3*a^2)
    q = (2*b^3-9*a*b*c+27*a^2*d)/(27*a^3)
    in map (subtract (b/(3*a))) $ solveDCubic p q