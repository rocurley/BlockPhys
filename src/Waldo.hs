{-# LANGUAGE LambdaCase  #-}

module Waldo where

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
import Utils

popCci :: CConVal -> State World Int
popCci grounded = do
    i:is <- use cCis
    cCis.=is
    cCons%=H.insert i grounded
    return i

pushCci :: Int -> State World ()
pushCci i = cCis%=(i:)

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

connectedNeighbors :: BlockKey -> Reader World [BlockKey]
connectedNeighbors blockKey = do
    links' <- view links --TODO: MAKE THIS NOT DUMB
    return [blockKey | (blockKey,linkKey) <- H.elems $ possibleLinks blockKey,
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

startJump :: Player -> Player
startJump (Player (Standing (BlockKey (x,y)) xOffset vx _)) =
    Player $ Jumping (fromIntegral x + xOffset,fromIntegral y) (vx,vJump) g
startJump (Player (NewlyFalling (x,y) (vx,vy) _)) =
    Player $ Jumping (x,y) (vx,vJump+vy) g
startJump plr = plr

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

cycleBlock :: BlockKey -> State World ()
cycleBlock key  = do
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

toggleLink :: LinkKey -> State World ()
toggleLink linkKey = do
    link <- use $ links.at linkKey
    success <- case link of
        Just OffLink    -> linkOn linkKey force0
        Just (OnLink _) -> linkOff linkKey
        Nothing -> error "toggleLink got a missing Link"
    unless success $ return $ error "link On/Off failed despite lookup success"
    setForces

stepWorld :: Time -> World -> World
stepWorld dt = execState (stepWorld' dt) where
  stepWorld' :: Time -> State World ()
  stepWorld' dt = do
      get >>= (player.playerMovement $ asState . timeEvolvePlayerMovement dt) >>= put
