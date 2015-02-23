{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE DataKinds #-}

import Prelude hiding (foldr,foldl,mapM,mapM_,sequence,concatMap)

import Graphics.Gloss.Interface.Pure.Game

import Data.List hiding (foldr,foldl,foldl',concatMap)
import Data.Monoid
import Data.Foldable
import Data.Fixed
import Data.Traversable
import Data.Maybe

import Control.Arrow
import Control.Applicative
import Control.Monad hiding (mapM,mapM_)
import Control.Monad.State.Strict hiding (mapM,mapM_)
import Control.Monad.Trans.Maybe

import Debug.Trace

import qualified Data.Map as H
import qualified Data.Set as S

import qualified Numeric.LinearAlgebra.HMatrix as LA
import Numeric.LinearAlgebra.Static as SLA
import qualified Data.Packed.Vector as V
import GHC.Float

import Control.Lens

import World
import Physics

main = play displayMode white 60
    initialWorld
    renderWorld
    handleEvent
    (const id)
 
--TODO:

--There's a bug in the block connection code.
--Need to reset stress to 0 when a group is ungrounded.
--The block coordinate system uses integers, but falling pieces can be at
--    intermediate positions.

displayMode = InWindow "Hello World" (560,560) (1000,50)
scaleFactor = 80
blockSize = 0.95
initialWorld = World (H.singleton (BlockKey (0,0)) (BlockVal Bedrock 0))
    H.empty (H.singleton 0 1) [1..]

renderWorld :: World -> Picture
renderWorld world@(World blocks links _ _)= let
    blockPictures = Pictures $ (`evalState` world) $ mapM renderBlock $ H.keys blocks
    stressPictures = Pictures $ (`evalState` world) $ mapM renderBlockStress $ H.keys blocks
    linkPictures = Pictures $ map renderLink $ H.toList links
    debug = scale scaleFactor scaleFactor $ Pictures
                  [Line [(0,0),(1,1)],Line [(0,1),(1,0)], Line [(0,0),(1,0),(1,1),(0,1),(0,0)]]
    testTrajectory = Parabola (-1,-1) (1,1) (-1)
    trajectoryPicture = renderTrajectory 280 280 testTrajectory
    playerCollision = (`evalState` world) $ predictCollision testTrajectory 5
    grid = Scale scaleFactor scaleFactor $ Pictures $
        [Line [(x,-3),(x,3)]|x<-[-3..3]] ++ [Line [(-3,y),(3,y)]|y<-[-3..3]]
    in Pictures $ [grid,blockPictures,linkPictures,stressPictures,trajectoryPicture] ++
        case playerCollision of
            Nothing -> [] 
            Just (Collision (x,y) t collidedBlock) -> [renderPlayer (x,y)]
handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ pt) = execState $ do
    linkClicked <- linkClickCheck pt
    case linkClicked of 
        Just linkKey -> handleLinkClick linkKey
        Nothing -> handleBlockClick (BlockKey $ roundToIntPoint pt)
handleEvent _ = id

handleBlockClick :: BlockKey -> State World ()
handleBlockClick key  = do
    blockVal <- use $ blocks.at key
    cycleBlock (key,blockVal)
    setForces
    --blockKeys <- use $ blocks.to H.keys
    --blockStresses <- traverse blockStress blockKeys
    --links' <- use links
    --traceShow links' $ return ()
    --traceShow (H.fromList $ zip blockKeys blockStresses) $ return ()

    where cycleBlock :: (BlockKey,Maybe BlockVal) -> State World ()
          cycleBlock (key,Nothing) = do
              cci <- popCci 0
              addBlock (key,BlockVal Normal cci)
          cycleBlock (key,Just (BlockVal Normal cci)) = do
              cCons.at cci %= fmap (+1)
              assign (blocks.at key) $ Just $ BlockVal Bedrock cci
          cycleBlock (key,Just (BlockVal Bedrock cci)) = void $ removeBlock key

handleLinkClick :: LinkKey -> State World ()
handleLinkClick linkKey = do
    link <- use $ links.at linkKey
    case link of
        Just OffLink    -> linkOn linkKey force0
        Just (OnLink _) -> linkOff linkKey
        Nothing -> error "handleLinkClick got a missing Link"
    setForces

linkOff :: LinkKey -> State World Bool
linkOff linkKey = fmap isJust $ runMaybeT $ do
    linkVal <- MaybeT $ use $ links.at linkKey
    lift $ assign (links.at linkKey) $ Just OffLink
    let (blockA,blockB) = linkedBlocks linkKey
    (BlockVal _ ccKeyA) <- MaybeT $ use $ blocks.at blockA
    grounded <- MaybeT $ use $ cCons.at ccKeyA
    (connectedToB,bGrounded) <- lift $ subgraph blockB
    unless (S.member blockA connectedToB) $ do
        lift $ cCons.at ccKeyA %= fmap (subtract bGrounded)
        cciB <- lift $ popCci bGrounded
        lift $ blocks.atMulti connectedToB.traverse.cci.= cciB

linkOn :: LinkKey -> Force -> State World Bool
linkOn linkKey force = fmap isJust $ runMaybeT $ do
    linkVal <- MaybeT $ use $ links.at linkKey
    let (blockA,blockB) = linkedBlocks linkKey
    BlockVal _ cciA <- MaybeT $ use $ blocks.at blockA 
    BlockVal _ cciB <- MaybeT $ use $ blocks.at blockB
    aGrounded <- MaybeT $ use $ cCons.at cciA
    (connectedToB,bGrounded) <- lift $ subgraph blockB
    lift $ assign (links.at linkKey) $ Just $ OnLink force
    unless (cciA == cciB) $ do
        lift $ assign (cCons.at cciA.traverse) (aGrounded + bGrounded)
        lift $ pushCci cciB
        lift $ blocks.atMulti connectedToB.traverse.cci.= cciA

renderBlock :: BlockKey -> State World Picture
renderBlock blockKey@(BlockKey (xi,yi)) = do
    BlockVal blockType cci <- fromJust <$> use (blocks.at blockKey)
    grounded <- (>0) <$> fromJust <$> use (cCons.at cci)
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
        translate x y $
        Pictures [box]

renderBlockStress :: BlockKey -> State World Picture
renderBlockStress blockKey@(BlockKey (xi,yi)) = do
    blockVal <- use $ blocks.at blockKey
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

linkClickCheck :: Point -> State World (Maybe LinkKey)
linkClickCheck (x,y) = let
    (xi,xrem) = divMod' (x/scaleFactor) 1
    (yi,yrem) = divMod' (y/scaleFactor) 1
    u = xrem + yrem -1
    v = yrem - xrem
    linkTester :: Point -> LinkKey -> State World (Maybe LinkKey)
    linkTester (x,y) linkKey = do
        linkVal <- use $ links.at linkKey
        return $ if inDiamond (x,y) && isJust linkVal
                 then Just linkKey
                 else Nothing
    in case (compare u 0,compare v 0) of
        (LT,LT) -> linkTester (xrem,yrem)   $ Link L2R (xi,yi) 
        (LT,GT) -> linkTester (yrem,xrem)   $ Link D2U (xi,yi) 
        (GT,LT) -> linkTester (yrem,1-xrem) $ Link D2U (xi+1,yi) 
        (GT,GT) -> linkTester (xrem,1-yrem) $ Link L2R (xi,yi+1) 
        _ -> return Nothing -- Nothing on the boundary

inDiamond :: Point -> Bool
inDiamond (x,y) = x'+y'<0.5 && x' < 0.2 && y' < 0.5
    where
        (x',y') = (abs $ (x-0.5)/0.8, abs $ y/0.8)

possibleLinks :: BlockKey -> H.Map Direction (BlockKey,LinkKey)
possibleLinks (BlockKey (x,y)) = H.fromList 
                                 [(RtDir, (BlockKey (x+1,y),Link L2R (x  ,y  ))),
                                  (LfDir, (BlockKey (x-1,y),Link L2R (x-1,y  ))),
                                  (DnDir, (BlockKey (x,y-1),Link D2U (x  ,y-1))),
                                  (UpDir, (BlockKey (x,y+1),Link D2U (x  ,y  )))]

removeBlock :: BlockKey -> State World Bool
removeBlock blockKey = fmap isJust $ runMaybeT $ do
    lift $ traverse (linkOff . snd) $ possibleLinks blockKey
    BlockVal _ cci <- MaybeT $ use $ blocks.at blockKey
    lift $ links.atMulti (snd <$> possibleLinks blockKey).= Nothing
    lift $ blocks.at blockKey.= Nothing
    lift $ pushCci cci

addBlock :: Block -> State World ()
addBlock (blockKey @(BlockKey (x,y)),val) = do
    blocks.at blockKey.= Just val
    traverse_ addLink $ possibleLinks blockKey
    where
        addLink (blockKey,linkKey) = do
            blockVal <- use $ blocks.at blockKey
            when (isJust blockVal) $ links.at linkKey.= Just OffLink

roundToIntPoint :: Point -> IntPt
roundToIntPoint (x,y) = (round (x/scaleFactor), round (y/scaleFactor))

connectedNeighbors :: BlockKey -> State World [BlockKey]
connectedNeighbors blockKey = do
    links' <- use links --TODO: MAKE THIS NOT DUMB
    return [blockKey|(blockKey,linkKey) <- H.elems $ possibleLinks blockKey,
        case H.lookup linkKey links' of
            Just (OnLink _) -> True
            _ -> False]

subgraph :: BlockKey -> State World (S.Set BlockKey,Int)
subgraph blockKey = dfs [blockKey] (S.empty,0) where
    isBedrock :: BlockKey -> State World Int
    isBedrock blockKey = do
        block <- use $ blocks.at blockKey
        case block of
            Just (BlockVal Bedrock _) -> return 1
            Just (BlockVal _ _) -> return 0 
    dfs :: [BlockKey] -> (S.Set BlockKey,Int) -> State World (S.Set BlockKey,Int)
    dfs [] out = return out
    dfs (x:xs) (visited,grounded)
        |x `S.member` visited = dfs xs (visited,grounded)
        |otherwise = do
            new <- connectedNeighbors x
            newGrounded <- (grounded +) <$> isBedrock x
            let acc = (S.insert x visited, newGrounded)
            dfs (new ++ xs) acc

linkGrounded :: LinkKey -> State World Bool
linkGrounded key = do
    maybeLinkVal <- use $ links.at key
    case maybeLinkVal of
        Nothing -> return False
        Just OffLink -> return False
        Just _ -> do
            let (blockKey,_) = linkedBlocks key
            Just (BlockVal _ cci) <- use $ blocks.at blockKey
            uses (cCons.at cci) $ \case
                Nothing -> error "cci not found"
                Just 0  -> False
                _       -> True

setForces :: State World ()
setForces = do
    blocksGrounded <- traverse (
        \ (BlockVal blockType cci) -> do
            Just cc <- use $ cCons.at cci 
            return (blockType==Bedrock,cc)
        ) =<< use blocks
    let blockForces = const g <$> H.filter (
            \ (isBedrock,nGroundings) ->
                not isBedrock && nGroundings > 0
            )blocksGrounded
    activeLinks <- filterM linkGrounded =<< (H.keys <$> use links)
    let forces = solveForces blockForces activeLinks
    traverse_ (\ (linkKey,force) -> let
        updateLink OffLink = OffLink
        updateLink (OnLink _) = OnLink force
        in links.at linkKey%= fmap updateLink) $ H.toList forces

blockStress :: BlockKey -> State World Stress
blockStress key = do
    maybeLinkVals <- H.toList <$> traverse (\ key -> use (links.at key)) (snd <$> possibleLinks key)
    return $ stressFromLinks [(dir,linkVal)|(dir,Just linkVal) <- maybeLinkVals]

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
    renderStressComp mag u v = let
        baseArrow = drawArrow u v $ double2Float (mag/4)
        (x,y) = vector2Point u
        arrow = case compare mag 0 of
            GT -> Translate (x/8) (y/8) baseArrow
            EQ -> Blank
            LT -> Translate (-x*(1/4+1/8)) (-y*(1/4+1/8)) baseArrow
        in Pictures [arrow, Rotate 180 arrow]
    in Pictures [renderStressComp lu u v,renderStressComp lv v u]

renderTrajectory :: Float -> Float -> Trajectory -> Picture

renderTrajectory xLim yLim trajectory@(Parabola (x0,y0) (vx,vy) ay) =
    case (compare vx 0,compare ay 0, compare vy 0) of
        (LT,_,_) -> Line [(x,yOfX x)|x<-[-xLim..x0f]]
        (GT,_,_) -> Line [(x,yOfX x)|x<-[x0f..xLim]]
        (EQ,LT,_) -> Line [(x0f,yPeak),(x0f,-yLim)] 
        (EQ,GT,_) -> Line [(x0f,yPeak),(x0f,yLim)] 
        (EQ,EQ,LT) -> Line [(x0f,y0f),(x0f,-yLim)] 
        (EQ,EQ,GT) -> Line [(x0f,y0f),(x0f,yLim)] 
        (EQ,EQ,EQ) -> Blank
    where
        x0f = x0*scaleFactor
        y0f = y0*scaleFactor
        tPeak = -vy/ay
        yPeak = scaleFactor * snd (atT trajectory tPeak)
        yOfX x = scaleFactor * snd (atT trajectory $ (x/scaleFactor-x0)/vx)
renderPlayer :: Point -> Picture
renderPlayer (x,y) = Scale scaleFactor scaleFactor $ Translate x y $
    Polygon [(0.2,0.4),(-0.2,0.4),(-0.2,-0.4),(0.2,-0.4)]