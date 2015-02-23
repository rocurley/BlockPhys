{-# LANGUAGE DataKinds #-}

module Physics
( solveForces
, g
, Stress(..)
, stressFromLinks
, Trajectory(..)
, Collision(..)
, atT
, predictCollision) where

import Prelude hiding (foldr,foldl,mapM,mapM_,sequence,minimum,maximum)

import Graphics.Gloss.Interface.Pure.Game

import Numeric.Minimization.QuadProgPP
import qualified Data.Packed.Matrix as M
import qualified Data.Map as H
import Data.Packed.Matrix ((><))
import qualified Data.Packed.Vector as V
import qualified Numeric.LinearAlgebra.HMatrix as LA
import GHC.TypeLits
import Numeric.LinearAlgebra.Static as SLA

import qualified Data.Set as S
import Data.List hiding (foldr,foldl,foldl',minimum,maximum)
import Data.Traversable
import Control.Monad hiding (mapM,mapM_)
import Control.Monad.State.Strict hiding (mapM,mapM_)
import Data.Monoid
import Data.Foldable
import Data.Maybe
import Data.Ord
import Safe

import Control.Lens

import Debug.Trace

import World

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
solveForces externalForces links = let
    nBlocks    = H.size externalForces :: Int
    nLinks     = length links
    blocksList = H.keys externalForces :: [BlockKey]
    blockMap   = H.fromList $ zip blocksList [0..] :: H.Map BlockKey Int
    orderedForces = map (externalForces H.!)  blocksList
    a = ident $ 3*nLinks
    b = V.fromList $ replicate (3*nLinks) 0
    c = M.fromColumns $ foldr (addLinkCols nBlocks blockMap) [] links
    --A fold would do this without list concatenation
    d = V.fromList $ join [[up, right, rotRight]|Force up right rotRight <- orderedForces]
    (forcesVector, energy) = case solveQuadProg (a,b) (Just (c,d)) Nothing of
        Right (f,e) -> (f,e)
        Left err -> error $ show err
    in H.fromList $ zip links $ unfoldr expandForcesList (V.toList forcesVector)

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

displayStress :: Stress -> Picture
displayStress (Stress stressMatrix) = let
    (eigenValues,eigenVectors) = SLA.eigensystem $ sym stressMatrix
    eigenPairs = zip (LA.toList $ SLA.extract eigenValues) (SLA.toColumns eigenVectors) :: [(Double,R 2)]
    in undefined

type Time = Float
data Trajectory = Parabola Point (Float,Float) Float deriving (Show,Eq,Ord)
data Collision  = Collision Point Time BlockKey deriving (Show,Eq,Ord)

atT :: Trajectory -> Time -> Point
atT (Parabola (x,y) (vx,vy) ay) t =
    (vx*t+x,1/2*ay*t^2 + vy*t + y)

xint :: Float -> Trajectory -> [(Point,Time)]
xint lineY trajectory@(Parabola (x,y) (vx,vy) ay) = let
    -- 0 = 1/2*ay*t^2 + vy*t + (y-lineY)
    discriminant = vy^2-2*ay*(y-lineY)
    in case compare discriminant 0 of
        GT -> do
            pm <- [(+),(-)]
            let t = (-vy `pm` sqrt (discriminant))/ay
            return (atT trajectory t, t)
        EQ -> let t = -vy/ay in return (atT trajectory t, t) 
        LT -> []

yint :: Float -> Trajectory -> [(Point,Time)]
yint lineX trajectory@(Parabola (x,y) (vx,vy) ay) = let
    t =(lineX-x)/vx 
    in [(atT trajectory t, t)]

trajectoryBox :: Trajectory -> Time -> ((Float,Float),(Float,Float))
trajectoryBox (Parabola (x,y) (vx,vy) ay) dt = let
    xf = x + vx*dt
    yf = y + vy*dt + ay*dt^2/2
    tPeak = -vy/ay
    yPeak = y + vy*tPeak + ay*tPeak^2/2
    yBounds = if (ay/=0) && (tPeak < dt)
        then (minimum [y,yf,yPeak],maximum [y,yf,yPeak])
        else (min y yf,max y yf)
    xBounds = (min x xf,max x xf)
    in (xBounds,yBounds)

predictCollision :: Trajectory -> Float -> State World (Maybe Collision)
predictCollision trajectory@(Parabola (x,y) (vx,vy) ay) dt = do
    let ((xmin,xmax),(ymin,ymax)) = trajectoryBox trajectory dt
    let (w,h) = (0.4,0.8)
    let blockCandidates = [BlockKey (x,y)|x<-[ceiling (xmin-(w+1)/2)..floor (xmax+(w+1)/2)],
                                          y<-[ceiling (ymin-(h+1)/2)..floor (ymax+(h+1)/2)]]
    blocksInBox <- filterM (\ block -> use $ blocks.to (H.member block)) blockCandidates
    let collisions = do --List monad
            block@(BlockKey (xBlockInt,yBlockInt)) <- blocksInBox
            let (xBlock,yBlock) = (fromIntegral xBlockInt,fromIntegral yBlockInt)
            pm <- [(+),(-)]
            (collisions,collisionChecker) <- [
                (xint (yBlock `pm` ((h+1)/2)) trajectory,
                    \ collisionX _ -> abs(xBlock-collisionX) < (w+1)/2),
                (yint (xBlock `pm` ((w+1)/2)) trajectory,
                    \ _ collisionY -> abs(yBlock-collisionY) < (h+1)/2)]
            ((collisionX,collisionY),collisionT) <- traceShowId $ collisions
            if collisionChecker collisionX collisionY && collisionT >0
            then [Collision (collisionX,collisionY) collisionT block]
            else []
    return $ minimumByMay (comparing (\ (Collision _ t _) -> t)) collisions
