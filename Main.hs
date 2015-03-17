{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase  #-}

import Prelude hiding (foldr,foldl,mapM,mapM_,sequence,concatMap)

import Graphics.Gloss.Interface.Pure.Game

import Data.Foldable
import Data.Fixed
import Data.Traversable
import Data.Maybe

import Control.Applicative
import Control.Monad hiding (mapM,mapM_)
import Control.Monad.State.Strict hiding (mapM,mapM_)
import Control.Monad.Reader hiding (mapM,mapM_)
import Control.Monad.Trans.Maybe

--import Debug.Trace

import qualified Data.Map as H
import qualified Data.Set as S

import Control.Lens

import World
import Physics
import Render

main :: IO ()
main = play displayMode white 60
    initialWorld
    (runReader renderWorld)
    handleEvent
    (const id) --stepWorld
 
--TODO:

--There's a bug in the block connection code.
--Need to reset stress to 0 when a group is ungrounded.
--The block coordinate system uses integers, but falling pieces can be at
--    intermediate positions.
--Need to establish exactly what our units are.
--Dry out the terrible huge list in predictcollision

displayMode :: Display
displayMode = InWindow "Hello World" (560,560) (1000,50)
initialPlayer :: Player
initialPlayer = undefined
initialWorld :: World
initialWorld = World (H.singleton (BlockKey (1,-3)) (BlockVal Bedrock 0))
    H.empty (H.singleton 0 1) [1..] initialPlayer

asState :: Reader s a -> State s a
asState rdr = runReader rdr <$> get

stepWorld :: Time -> World -> World
stepWorld dt = execState (stepWorld' dt)

stepWorld' :: Time -> State World ()
stepWorld' dt = undefined

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ pt) = execState $ do
    linkClicked <- asState $ linkClickCheck pt
    case linkClicked of 
        Just linkKey -> handleLinkClick linkKey
        Nothing -> handleBlockClick (BlockKey $ roundToIntPoint pt)
handleEvent _ = id

handleBlockClick :: BlockKey -> State World ()
handleBlockClick key  = do
    blockVal <- use $ blocks.at key
    case blockVal of
        Nothing -> do
            cci <- popCci 0
            addBlock (key,BlockVal Normal cci)
        Just (BlockVal Normal cci) -> do
            cCons.at cci %= fmap (+1)
            assign (blocks.at key) $ Just $ BlockVal Bedrock cci
        Just (BlockVal Bedrock cci) -> void $ removeBlock key
    setForces

handleLinkClick :: LinkKey -> State World ()
handleLinkClick linkKey = do
    link <- use $ links.at linkKey
    success <- case link of
        Just OffLink    -> linkOn linkKey force0
        Just (OnLink _) -> linkOff linkKey
        Nothing -> error "handleLinkClick got a missing Link"
    unless success $ return $ error "link On/Off failed despite lookup success"
    setForces

linkOff :: LinkKey -> State World Bool
linkOff linkKey = fmap isJust $ runMaybeT $ do
    linkVal <- MaybeT $ use $ links.at linkKey
    lift $ assign (links.at linkKey) $ Just OffLink
    let (blockA,blockB) = linkedBlocks linkKey
    (BlockVal _ ccKeyA) <- MaybeT $ use $ blocks.at blockA
    grounded <- MaybeT $ use $ cCons.at ccKeyA
    (connectedToB,bGrounded) <- lift $ asState $ subgraph blockB
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
    (connectedToB,bGrounded) <- lift $ asState $ subgraph blockB
    lift $ assign (links.at linkKey) $ Just $ OnLink force
    unless (cciA == cciB) $ do
        lift $ assign (cCons.at cciA.traverse) (aGrounded + bGrounded)
        lift $ pushCci cciB
        lift $ blocks.atMulti connectedToB.traverse.cci.= cciA

linkClickCheck :: Point -> Reader World (Maybe LinkKey)
linkClickCheck (x,y) = let
    (xi,xrem) = divMod' (x/scaleFactor) 1
    (yi,yrem) = divMod' (y/scaleFactor) 1
    u = xrem + yrem -1
    v = yrem - xrem
    linkTester :: Point -> LinkKey -> Reader World (Maybe LinkKey)
    linkTester (x,y) linkKey = do
        linkVal <- view $ links.at linkKey
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

removeBlock :: BlockKey -> State World Bool
removeBlock blockKey = fmap isJust $ runMaybeT $ do
    _ <- lift $ traverse (linkOff . snd) $ possibleLinks blockKey
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

connectedNeighbors :: BlockKey -> Reader World [BlockKey]
connectedNeighbors blockKey = do
    links' <- view links --TODO: MAKE THIS NOT DUMB
    return [blockKey|(blockKey,linkKey) <- H.elems $ possibleLinks blockKey,
        case H.lookup linkKey links' of
            Just (OnLink _) -> True
            _ -> False]

subgraph :: BlockKey -> Reader World (S.Set BlockKey,Int)
subgraph blockKey = dfs [blockKey] (S.empty,0) where
    isBedrock :: BlockKey -> Reader World Int
    isBedrock blockKey = do
        block <- view $ blocks.at blockKey
        case block of
            Just (BlockVal Bedrock _) -> return 1
            Just (BlockVal _ _) -> return 0 
            Nothing -> error "blockKey not found in blockMap"
    dfs :: [BlockKey] -> (S.Set BlockKey,Int) -> Reader World (S.Set BlockKey,Int)
    dfs [] out = return out
    dfs (x:xs) (visited,grounded)
        |x `S.member` visited = dfs xs (visited,grounded)
        |otherwise = do
            new <- connectedNeighbors x
            newGrounded <- (grounded +) <$> isBedrock x
            let acc = (S.insert x visited, newGrounded)
            dfs (new ++ xs) acc

linkGrounded :: LinkKey -> Reader World Bool
linkGrounded key = do
    maybeLinkVal <- view $ links.at key
    case maybeLinkVal of
        Nothing -> return False
        Just OffLink -> return False
        Just _ -> do
            let (blockKey,_) = linkedBlocks key
            Just (BlockVal _ cci) <- view $ blocks.at blockKey
            views (cCons.at cci) $ \case
                Nothing -> error "cci not found"
                Just 0  -> False
                _       -> True

setForces :: State World ()
setForces = do
    blocksGrounded <- traverse (
        \ (BlockVal blockType blockCCi) -> do
            Just cc <- use $ cCons.at blockCCi 
            return (blockType==Bedrock,cc)
        ) =<< use blocks
    let blockForces = const fg <$> H.filter (
            \ (isBedrock,nGroundings) ->
                not isBedrock && nGroundings > 0
            )blocksGrounded
    activeLinks <- asState $ filterM linkGrounded =<< (H.keys <$> view links)
    let forces = solveForces blockForces activeLinks
    traverse_ (\ (linkKey,force) -> let
        updateLink OffLink = OffLink
        updateLink (OnLink _) = OnLink force
        in links.at linkKey%= fmap updateLink) $ H.toList forces

startJump :: Player -> Player
startJump (Player (Standing (BlockKey (x,y)) xOffset vx _)) =
    Player $ Jumping (fromIntegral x + xOffset,fromIntegral y) (vx,vJump) g 
startJump (Player (NewlyFalling (x,y) (vx,vy) _)) = 
    Player $ Jumping (x,y) (vx,vJump+vy) g
startJump plr = plr
