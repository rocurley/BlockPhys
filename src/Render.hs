{-# LANGUAGE DataKinds #-}

module Render
( renderWorld
, renderBlock
, renderBlockStress
, renderLink
, renderStress
, stressFromLinks
, renderTrajectory
, renderPlayer) where

import Graphics.Gloss.Interface.Pure.Game

import qualified Numeric.LinearAlgebra.HMatrix as LA
import Numeric.LinearAlgebra.Static as SLA
import qualified Data.Packed.Vector as V
import GHC.Float

import Data.Maybe

import Debug.Trace

import Control.Applicative
import Control.Monad.Reader hiding (mapM,mapM_)

import qualified Data.Map as H

import qualified Map2D

import Control.Lens

import Physics
import Waldo
import World

renderWorld :: Reader World Picture
renderWorld = do
    blockPictures  <- fmap Pictures $ mapM renderBlock =<< view (blocks.to Map2D.keys)
    stressPictures <- fmap Pictures $ mapM renderBlockStress =<< view (blocks.to Map2D.keys)
    linkPictures   <- Pictures <$> map renderLink <$> H.toList <$> view links
    playerPicture  <- renderPlayer <$> view player
    futurePlayers <- traverse (\ t -> do
        ((playerMovement%~unJump) <$> view player) >>= playerMovement (timeEvolvePlayerMovement t)
        ) [0,0.1..5]
    let playerFuturePictures = Pictures $ renderPlayer <$> futurePlayers
    let debug = scale scaleFactor scaleFactor $ Pictures
                  [Line [(0,0),(1,1)],Line [(0,1),(1,0)], Line [(0,0),(1,0),(1,1),(0,1),(0,0)]]
    let grid = Scale scaleFactor scaleFactor $ Pictures $
            [Line [(x,-3),(x,3)]|x<-[-3..3]] ++ [Line [(-3,y),(3,y)]|y<-[-3..3]]
    return $ Pictures $ [grid,blockPictures,linkPictures,stressPictures,playerPicture,playerFuturePictures]

renderBlock :: IntPt -> Reader World Picture
renderBlock blockKey@(xi,yi) = do
    BlockVal blockType cci <- fromJust <$> view (blocks.at blockKey)
    grounded <- (>0) <$> fromJust <$> view (cCons.at cci)
    let (x,y) = (fromIntegral xi, fromIntegral yi)
    let c = case (blockType,grounded) of
            (Normal,True)  -> greyN 0.3
            (Normal,False) -> greyN 0.6
            (Bedrock,_)    -> black
    let box = scale blockSize blockSize $ color c $
                Polygon [(-0.5,-0.5),(0.5,-0.5),(0.5,0.5),(-0.5,0.5)]
    let cciLabel = translate (-0.25) (-0.3) $
                        scale (0.5/scaleFactor) (0.5/scaleFactor) $
                        color red $ Text $ show cci
    return $ scale scaleFactor scaleFactor $
        translate x y $ box

renderBlockStress :: IntPt -> Reader World Picture
renderBlockStress blockKey@(xi,yi) = do
    blockVal <- view $ blocks.at blockKey
    case blockVal of
        Nothing -> error "Trying to render an invalid blockKey"
        Just (BlockVal Bedrock _) -> return Blank
        _ -> do
            stress <- blockStress blockKey
            let (x,y) = (fromIntegral xi, fromIntegral yi)
            return $ scale scaleFactor scaleFactor $ translate x y $ renderStress stress

renderLink :: Link -> Picture
renderLink (Link linkDirection (xi,yi),linkVal) = color c $
    scale scaleFactor scaleFactor $ translate x y d
    where x = fromIntegral xi
          y = fromIntegral yi
          c = case linkVal of
              OffLink  -> makeColor 1 0 0 0.3
              OnLink _ -> red
          d = case linkDirection of
              L2R -> diamond
              D2U -> rotate (-90) diamond

diamond = translate 0.5 0 $ scale 0.8 0.8 $
          Polygon [(-l,0.5-l),(0,0.5),(l,0.5-l),
                   (l,-0.5+l),(0,-0.5),(-l,-0.5+l)]
          where l = 0.2

vector2Point :: R 2 -> Point
vector2Point = (\ [x,y] -> (double2Float x,double2Float y)) . V.toList . SLA.extract

drawArrow :: R 2 -> R 2 -> Float -> Picture
drawArrow u v width' = scale (1/4) (1/4) $ Pictures [box,triangle]
    where
        width = vector [float2Double width',float2Double width'] :: R 2
        diffsToPolygon :: R 2 -> [R 2] -> Picture
        diffsToPolygon start = Polygon . map vector2Point . scanl (+) start
        box = diffsToPolygon (u/2+(width/2)*v)
            ([ (3/5)*u,
              (-width)*v,
              (-3/5)*u] :: [R 2])
        triangle = diffsToPolygon (u/2+width*v+(3/5)*u)
            ([ (2/5)*u - width*v,
              (-2/5)*u -width*v] :: [R 2])

renderStress :: Stress -> Picture
renderStress  (Stress stressMatrix) = let
    (eigenValues,eigenVectors) = SLA.eigensystem $ sym stressMatrix
    [u,v] = SLA.toColumns eigenVectors
    [lu,lv] = LA.toList $ SLA.extract eigenValues
    renderStressComp :: Double -> R 2 -> R 2 -> Picture
    renderStressComp mag u' v' = let
        baseArrow = drawArrow u' v' $ double2Float (mag/4)
        (x,y) = vector2Point u'
        arrow = case compare mag 0 of
            GT -> Translate (x/8) (y/8) baseArrow
            EQ -> Blank
            LT -> Translate (-x*(1/4+1/8)) (-y*(1/4+1/8)) baseArrow
        in Pictures [arrow, Rotate 180 arrow]
    in Pictures [renderStressComp lu u v,renderStressComp lv v u]

renderTrajectory :: Float -> Float -> Trajectory -> Picture
renderTrajectory xLim _ trajectory =
    case compare vx 0 of
        LT -> Line [(x,yOfX x)|x<-[-xLim..x0f]]
        GT -> Line [(x,yOfX x)|x<-[x0f..xLim]]
        EQ -> Blank --Screw all that
    where
        (x0,vx) = case trajectory of
            Parabola (x0',_) (vx',_) _ -> (x0', vx')
            JumpTrajectory (x0',_) (vx',_) _ _ _ -> (x0', vx')
        x0f = x0*scaleFactor
        yOfX x = scaleFactor * snd (startPoint $ atT trajectory $ (x/scaleFactor-x0)/vx)
renderPlayer :: Player -> Picture
renderPlayer player = let
  color = withAlpha 0.2 $ case player^.playerMovement of
            Standing{} -> green
            Jumping{} -> red
            Falling{} -> blue
            NewlyFalling{} -> orange
  (x,y) = player^.playerMovement.playerLoc
  playerShape = Polygon [(0.2,0.4),(-0.2,0.4),(-0.2,-0.4),(0.2,-0.4)]
  in Color color $ Scale scaleFactor scaleFactor $ Translate x y playerShape
