import Prelude hiding (foldr,foldl,mapM,mapM_,sequence)

import Graphics.Gloss.Interface.Pure.Game

import Data.List hiding (foldr,foldl,foldl')
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

main = play displayMode white 60
    initialWorld
    renderWorld
    handleEvent
    (const id)
 


--TODO:

--Make it possible to run the physics sim and display the output in some legible way
--Figure out a way to make the whole darn program not crash if there's no bedrock.
--The block coordinate system uses integers, but falling pieces can be at
--    intermediate positions.
--Modules! World sounds like a good place to start! Physics would be good too.



type IntPt = (Int,Int)

data BlockType = Normal | Bedrock deriving (Eq,Ord,Show)
newtype BlockKey = BlockKey IntPt deriving (Eq,Ord,Show)
data BlockVal = BlockVal BlockType Int deriving (Eq,Ord,Show)
type Block = (BlockKey,BlockVal)
type BlockMap = H.Map BlockKey BlockVal

--I don't like that this can represent invalid states, but the validitiy of a link is
--pretty much tied to the global state, so I don't think it can be cromulently enforced
--by the type system. (Why haven't you learned Agda again?)

data LinkKey = L2R IntPt | D2U IntPt deriving (Eq,Ord,Show)
newtype LinkVal = LinkVal {active :: Bool} deriving (Eq,Ord,Show)
type Link = (LinkKey,LinkVal)
type LinkMap  = H.Map LinkKey LinkVal


data Force = Force {up :: Double, right :: Double, rotCCW :: Double} deriving (Show)

type CConKey = Int
type CConVal = Int
type CCon = (CConKey,CConVal)
type CConMap = H.Map CConKey CConVal

data World = World BlockMap LinkMap CConMap [Int]

getBlocks :: State World BlockMap
getBlocks = do
    World blocks _ _ _ <- get
    return blocks

lookupBlock :: BlockKey -> State World (Maybe BlockVal)
lookupBlock blockKey = H.lookup blockKey <$> getBlocks

setBlock :: BlockKey -> Maybe BlockVal -> State World ()
setBlock blockKey blockVal = do
    World blocks links cCons cci <- get
    put $ World (H.alter (const blockVal) blockKey blocks) links cCons cci 

alterBlock :: BlockKey -> (Maybe BlockVal -> Maybe BlockVal) -> State World ()
alterBlock blockKey f = do
    World blocks links cCons cci <- get
    put $ World (H.alter f blockKey blocks) links cCons cci 

getLinks :: State World LinkMap
getLinks = do
    World _ links _ _ <- get
    return links

lookupLink :: LinkKey -> State World (Maybe LinkVal)
lookupLink linkKey = H.lookup linkKey <$> getLinks

setLink :: LinkKey -> Maybe LinkVal -> State World ()
setLink linkKey linkVal = do
    World blocks links cCons cci <- get
    put $ World blocks(H.alter (const linkVal) linkKey links) cCons cci 

alterLink :: LinkKey -> (Maybe LinkVal -> Maybe LinkVal) -> State World ()
alterLink linkKey f = do
    World blocks links cCons cci <- get
    put $ World blocks (H.alter f linkKey links) cCons cci 

popCci :: CConVal -> State World Int
popCci grounded = do
    World blocks links cCons (i:is) <- get
    put $ World blocks links (H.insert i grounded cCons) is
    return i

pushCci :: Int -> State World ()
pushCci i = do
    World blocks links cCons is <- get
    put $ World blocks links (H.delete i cCons) $ i:is

setCc :: Int -> CConVal -> State World ()
setCc i grounded = do
    World blocks links cCons is <- get
    put $ World blocks links (H.insert i grounded cCons) $ is

getCc :: Int -> State World (Maybe CConVal)
getCc i = do
    World _ _ cCons _ <- get
    return $ H.lookup i cCons

adjustCc :: (CConVal -> CConVal) -> CConKey -> State World ()
adjustCc f key = do
    World blocks links cCons is <- get
    put $ World blocks links (H.adjust f key cCons) is

displayMode = InWindow "Hello World" (560,560) (1000,50)
scaleFactor = 80
blockSize = 0.95
initialWorld = World (H.singleton (BlockKey (0,0)) (BlockVal Bedrock 0))
    H.empty (H.singleton 0 1) [1..]

renderWorld :: World -> Picture
renderWorld world@(World blocks links _ _)= Pictures [
    Pictures $ (`evalState` world) $ mapM renderBlock $ H.keys blocks,
    Pictures $ map renderLink $ H.toList links,debug]
    where debug = scale scaleFactor scaleFactor $ Pictures
                  [Line [(0,0),(1,1)],Line [(0,1),(1,0)], Line [(0,0),(1,0),(1,1),(0,1),(0,0)]]

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ pt) = execState $ do
    linkClicked <- linkClickCheck pt
    case linkClicked of 
        Just linkKey -> handleLinkClick linkKey
        Nothing -> handleBlockClick (BlockKey $ roundToIntPoint pt)
handleEvent _ = id

handleBlockClick :: BlockKey -> State World ()
handleBlockClick key  = do
    world@(World blocks _ _ _) <- get
    cycleBlock (key,H.lookup key blocks)
    where cycleBlock :: (BlockKey,Maybe BlockVal) -> State World ()
          cycleBlock (key,Nothing) = do
              cci <- popCci 0
              addBlock (key,BlockVal Normal cci)
          cycleBlock (key,Just (BlockVal Normal cci)) = do
              adjustCc (+1) cci
              setBlock key $ Just (BlockVal Bedrock cci)
          cycleBlock (key,Just (BlockVal Bedrock cci)) = void $ removeBlock key

handleLinkClick :: LinkKey -> State World ()
handleLinkClick linkKey = do
    link <- lookupLink linkKey
    case link of --Should never get a Nothing.
        Just (LinkVal True)  -> linkOff linkKey
        Just (LinkVal False) -> linkOn  linkKey
    return ()

linkedBlocks :: LinkKey -> (BlockKey,BlockKey)
linkedBlocks (L2R (x,y)) = (BlockKey (x,y), BlockKey (x+1,y))
linkedBlocks (D2U (x,y)) = (BlockKey (x,y), BlockKey (x,y+1))

linkOff :: LinkKey -> State World Bool
linkOff linkKey = fmap isJust $ runMaybeT $ do
    linkVal <- MaybeT $ lookupLink linkKey
    lift $ setLink linkKey $ Just $ LinkVal False
    let (blockA,blockB) = linkedBlocks linkKey
    (BlockVal _ ccKeyA) <- MaybeT $ lookupBlock blockA
    grounded <- MaybeT $ getCc ccKeyA
    (connectedToB,bGrounded) <- lift $ subgraph blockB
    unless (S.member blockA connectedToB) $ do
        lift $ adjustCc (subtract bGrounded) ccKeyA
        cci <- lift $ popCci bGrounded
        lift $ mapM_ (`alterBlock` (\ (Just (BlockVal ty _)) ->
            Just $ BlockVal ty cci)) connectedToB

linkOn :: LinkKey -> State World Bool
linkOn linkKey = fmap isJust $ runMaybeT $ do
    linkVal <- MaybeT $ lookupLink linkKey
    let (blockA,blockB) = linkedBlocks linkKey
    BlockVal _ cciA <- MaybeT $ lookupBlock blockA 
    BlockVal _ cciB <- MaybeT $ lookupBlock blockB
    aGrounded <- MaybeT $ getCc cciA
    (connectedToB,bGrounded) <- lift $ subgraph blockB
    lift $ setLink linkKey $ Just $ LinkVal True
    unless (cciA == cciB) $ do
        lift $ setCc cciA (aGrounded + bGrounded)
        lift $ pushCci cciB
        lift $ mapM_ (`alterBlock` (\ ( Just (BlockVal ty _)) ->
            Just $ BlockVal ty cciA)) connectedToB

renderBlock :: BlockKey -> State World Picture
renderBlock blockKey@(BlockKey (xi,yi)) = do
    BlockVal blockType cci <- fromJust <$> lookupBlock blockKey
    grounded <- fromJust <$> getCc cci
    let (x,y) = (fromIntegral xi, fromIntegral yi)
    let c = case (blockType,grounded>0) of
            (Normal,True)  -> greyN 0.3
            (Normal,False) -> greyN 0.6
            (Bedrock,_)    -> black
    return $ scale scaleFactor scaleFactor $
        translate x y $
        Pictures [
        scale blockSize blockSize $ 
        color c $
        Polygon [(-0.5,-0.5),(0.5,-0.5),(0.5,0.5),(-0.5,0.5)],
        translate (-0.25) (-0.3) $ scale (0.5/scaleFactor) (0.5/scaleFactor) $
            color red $ Text $ show cci]

renderLink :: Link -> Picture
renderLink (L2R (xi,yi),LinkVal active) = color c $ scale scaleFactor scaleFactor $
    translate x y diamond
    where x = fromIntegral xi
          y = fromIntegral yi
          c = if active then red else blue
renderLink (D2U (xi,yi),LinkVal active) = color c $ scale scaleFactor scaleFactor $
    translate x y $ rotate (-90) diamond
    where x = fromIntegral xi
          y = fromIntegral yi
          c = if active then red else blue

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
    linkTester (x,y) link = do
        links <- getLinks
        return $ if inDiamond (x,y) && H.member link links
                 then Just link
                 else Nothing
    in case (compare u 0,compare v 0) of
        (LT,LT) -> linkTester (xrem,yrem)$ L2R (xi,yi) 
        (LT,GT) -> linkTester (yrem,xrem) $ D2U (xi,yi) 
        (GT,LT) -> linkTester (yrem,1-xrem)  $ D2U (xi+1,yi) 
        (GT,GT) -> linkTester (xrem,1-yrem) $ L2R (xi,yi+1) 
        _ -> return Nothing -- Nothing on the boundary

inDiamond :: Point -> Bool
inDiamond (x,y) = x'+y'<0.5 && x' < 0.2 && y' < 0.5
    where
        (x',y') = (abs $ (x-0.5)/0.8, abs $ y/0.8)

possibleLinks :: BlockKey -> [(BlockKey,LinkKey)]
possibleLinks (BlockKey (x,y)) = [(BlockKey (x+1,y),L2R (x  ,y  )),
                                  (BlockKey (x-1,y),L2R (x-1,y  )),
                                  (BlockKey (x,y-1),D2U (x  ,y-1)),
                                  (BlockKey (x,y+1),D2U (x  ,y  ))]

removeBlock :: BlockKey -> State World Bool
removeBlock blockKey = fmap isJust $ runMaybeT $ do
    lift $ mapM_ (linkOff . snd) $ possibleLinks blockKey
    BlockVal _ cci <- MaybeT $ lookupBlock blockKey
    lift $ mapM_ ((`setLink` Nothing) . snd ) $ possibleLinks blockKey
    lift $ setBlock blockKey Nothing
    lift $ pushCci cci

addBlock :: Block -> State World ()
addBlock (blockKey @(BlockKey (x,y)),val) = do
    setBlock blockKey $ Just val
    mapM_ addLink $ possibleLinks blockKey
    where
        addLink (blockKey,linkKey) = do
            blocks <- getBlocks
            when (blockKey `H.member` blocks)$ 
                setLink linkKey (Just $ LinkVal False) 

roundToIntPoint :: Point -> IntPt
roundToIntPoint (x,y) = (round (x/scaleFactor), round (y/scaleFactor))

connectedNeighbors :: BlockKey -> State World [BlockKey]
connectedNeighbors blockKey = do
    links <- getLinks
    return [blockKey|(blockKey,linkKey) <- possibleLinks blockKey,
        Just (LinkVal True) == H.lookup linkKey links]

subgraph :: BlockKey -> State World (S.Set BlockKey,Int)
subgraph blockKey = dfs [blockKey] (S.empty,0) where
    isBedrock :: BlockKey -> State World Int
    isBedrock blockKey = do
        block <- lookupBlock blockKey
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