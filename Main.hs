import Prelude hiding (foldr,foldl,mapM,mapM_,sequence)

import Graphics.Gloss.Interface.Pure.Game

import Data.List hiding (foldr,foldl,foldl')
import Data.Monoid
import Data.Foldable
import Data.Fixed
import Data.Traversable
import Control.Arrow
import Control.Monad hiding (mapM,mapM_)
import Control.Monad.State.Lazy hiding (mapM,mapM_)

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
-- Compute connected subgraphs starting at bedrocks, and color disconnected blocks
--    differently.
-- Refactor into the state monad






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

data World = World BlockMap LinkMap [Int]

getBlocks :: State World BlockMap
getBlocks = do
    World blocks _ _ <- get
    return blocks

setBlock :: BlockKey -> Maybe BlockVal -> State World ()
setBlock blockKey blockVal = do
    World blocks links cci <- get
    put $ World (H.alter (const blockVal) blockKey blocks) links cci 

alterBlock :: BlockKey -> (Maybe BlockVal -> Maybe BlockVal) -> State World ()
alterBlock blockKey f = do
    World blocks links cci <- get
    put $ World (H.alter f blockKey blocks) links cci 

getLinks :: State World LinkMap
getLinks = do
    World _ links _ <- get
    return links

setLink :: LinkKey -> Maybe LinkVal -> State World ()
setLink linkKey linkVal = do
    World blocks links cci <- get
    put $ World blocks(H.alter (const linkVal) linkKey links) cci 

popCci :: State World Int
popCci = do
    World blocks links (i:is) <- get
    put $ World blocks links is
    return i

pushCci :: Int -> State World ()
pushCci i = do
    World blocks links (is) <- get
    put $ World blocks links $ i:is

displayMode = InWindow "Hello World" (560,560) (1000,50)
scaleFactor = 80
blockSize = 0.95
initialWorld = World (H.singleton (BlockKey (0,0)) (BlockVal Bedrock 0)) H.empty [1..]

renderWorld :: World -> Picture
renderWorld (World blocks links _)= Pictures $ [Pictures $ map renderBlock $ H.toList blocks,
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
    world@(World blocks _ _) <- get
    put $ cycleBlock (key,H.lookup key blocks) world
    where cycleBlock :: (BlockKey,Maybe BlockVal) -> World -> World
          cycleBlock (key,Nothing) (World blocks links (cci:is)) =
              addBlock (key,BlockVal Normal cci) (World blocks links is)
          cycleBlock (key,Just (BlockVal Normal cci)) (World blocks links cciList) =
              World (H.insert key (BlockVal Bedrock cci) blocks) links cciList
          cycleBlock (key,Just (BlockVal Bedrock cci)) (World blocks links cciList) =
              removeBlock key (World blocks links (cci:cciList))

handleLinkClick :: LinkKey -> State World ()
handleLinkClick linkKey = do
    links <- getLinks
    case links H.! linkKey of
        LinkVal True  -> linkOff linkKey
        LinkVal False -> linkOn  linkKey

linkedBlocks :: LinkKey -> (BlockKey,BlockKey)
linkedBlocks (L2R (x,y)) = (BlockKey (x,y), BlockKey (x+1,y))
linkedBlocks (D2U (x,y)) = (BlockKey (x,y), BlockKey (x,y+1))

linkOff :: LinkKey -> State World ()
linkOff linkKey = do
    setLink linkKey $ Just $ LinkVal False
    links <- getLinks
    blocks <- getBlocks
    let (blockA,blockB) = linkedBlocks linkKey 
    let (connectedToB,bGrounded) = subgraph blocks links blockB
    if S.member blockA connectedToB
    then return ()
    else do
        cci <- popCci
        mapM_ (`alterBlock` (\ (Just (BlockVal ty _)) ->
            Just $ BlockVal ty cci)) connectedToB

linkOn :: LinkKey -> State World ()
linkOn linkKey = do
    links <- getLinks
    blocks <- getBlocks
    let (blockA,blockB) = linkedBlocks linkKey
    let BlockVal _ cciA = blocks H.! blockA 
    let BlockVal _ cciB = blocks H.! blockB
    let (connectedToB,bGrounded) = subgraph blocks links blockB
    setLink linkKey $ Just $ LinkVal True
    if cciA == cciB
    then return ()
    else do
        pushCci cciB
        mapM_ (`alterBlock` (\ ( Just (BlockVal ty _)) ->
            Just $ BlockVal ty cciA)) connectedToB

renderBlock :: Block -> Picture
renderBlock (BlockKey (xi,yi), BlockVal blockType cci) =
    scale scaleFactor scaleFactor $
    translate x y $
    Pictures [
    scale blockSize blockSize $ 
    color c $
    Polygon [(-0.5,-0.5),(0.5,-0.5),(0.5,0.5),(-0.5,0.5)],
    translate (-0.25) (-0.3) $ scale (0.5/scaleFactor) (0.5/scaleFactor) $
        color red $ Text $ show cci]
    where x = fromIntegral xi
          y = fromIntegral yi
          c = colorOf blockType
          colorOf Normal  = greyN 0.4
          colorOf Bedrock = black

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

removeBlock :: BlockKey -> World -> World
removeBlock blockKey (World blocks links cciList) =
    World (H.delete blockKey blocks)
        (foldl' (flip H.delete) links $ map snd $ possibleLinks blockKey)
        cciList

addBlock :: Block -> World -> World
addBlock (key @(BlockKey (x,y)),val) (World blocks links cciList) =
    World (H.insert key val blocks)
        (appEndo (foldMap addLink $ possibleLinks key) links)
        cciList
    where addLink (block,linkKey) = Endo $ if (block `H.member` blocks)
                                        then H.insert linkKey (LinkVal False)
                                        else id

roundToIntPoint :: Point -> IntPt
roundToIntPoint (x,y) = (round (x/scaleFactor), round (y/scaleFactor))

connectedNeighbors :: BlockKey -> LinkMap -> [BlockKey]
connectedNeighbors blockKey links =
    [blockKey|(blockKey,linkKey) <- possibleLinks blockKey,
        Just (LinkVal True) == H.lookup linkKey links]

subgraph :: BlockMap -> LinkMap -> BlockKey -> (S.Set BlockKey,Bool)
subgraph blocks links key = dfs [key] (S.empty,False) where
    isBedrock :: BlockVal -> Bool
    isBedrock (BlockVal Bedrock _) = True
    isBedrock (BlockVal _ _) = False
    dfs :: [BlockKey] -> (S.Set BlockKey,Bool) -> (S.Set BlockKey,Bool)
    dfs [] out = out
    dfs (x:xs) (visited,grounded)
        |x `S.member` visited = dfs xs (visited,grounded)
        |otherwise =
            let new = connectedNeighbors x links
                acc = (S.insert x visited, isBedrock (blocks H.! x) || grounded) 
                --acc = foldl' (\ (visited,grounded) blockKey ->
                --    (S.insert blockKey visited, grounded && (isBedrock $ blocks H.! blockKey)))
                --    (visited,grounded) new
                in dfs (new ++ xs) acc


partitionBlocks :: BlockMap -> LinkMap -> [(S.Set BlockKey,Bool)]
partitionBlocks blocks links
    |H.null blocks = []
    |otherwise = let (sg,grounded) = subgraph blocks links (head $ H.keys blocks)
                     in (sg,grounded) : partitionBlocks (foldl' (flip H.delete) blocks sg) links