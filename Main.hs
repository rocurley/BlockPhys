import Prelude hiding (foldr,foldl)

import Graphics.Gloss.Interface.Pure.Game

import Data.List hiding (foldr,foldl,foldl')
import Data.Monoid
import Data.Foldable
import Control.Arrow
import Control.Monad

import qualified Data.Map as H
import qualified Data.Set as S

main = play displayMode white 60
    initialWorld
    renderWorld
    handleEvent
    (const id)
 


--TODO:

--Make it possible to click on links to make them active/passive.
--Make it possible to run the physics sim and display the output in some legible way
--Figure out a way to make the whole darn program not crash if there's no bedrock.
--The block coordinate system uses integers, but falling pieces can be at
--    intermediate positions.






type IntPt = (Int,Int)
data World = World (H.Map BlockKey BlockVal) (H.Map LinkKey LinkVal)

data BlockType = Normal | Bedrock deriving (Eq,Ord,Show)
newtype BlockKey = BlockKey IntPt deriving (Eq,Ord,Show)
newtype BlockVal = BlockVal BlockType deriving (Eq,Ord,Show)
type Block = (BlockKey,BlockVal)

--I don't like that this can represent invalid states, but the validitiy of a link is
--pretty much tied to the global state, so I don't think it can be cromulently enforced
--by the type system. (Why haven't you learned Agda again?)

data LinkKey = L2R IntPt | D2U IntPt deriving (Eq,Ord,Show)
newtype LinkVal = LinkVal Bool deriving (Eq,Ord,Show)
type Link = (LinkKey,LinkVal)


data Force = Force {up :: Double, right :: Double, rotCCW :: Double} deriving (Show)

displayMode = InWindow "Hello World" (560,560) (1000,50)
scaleFactor = 80
blockSize = 0.95
initialWorld = World (H.singleton (BlockKey (0,0)) (BlockVal Bedrock)) H.empty

renderWorld :: World -> Picture
renderWorld (World blocks links)= Pictures $ [Pictures $ map renderBlock $ H.toList blocks,
    Pictures $ map renderLink $ H.toList links]

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ pt) world@(World blocks links) =
    cycleBlock (key,H.lookup key blocks) world
    where key = BlockKey $ roundToIntPoint pt
          cycleBlock :: (BlockKey,Maybe BlockVal) -> World -> World
          cycleBlock (key,Nothing) world = addBlock (key,BlockVal Normal) world
          cycleBlock (key,Just (BlockVal Normal)) (World blocks links) =
              World (H.insert key (BlockVal Bedrock) blocks) links
          cycleBlock (key,Just (BlockVal Bedrock)) world = removeBlock key world
handleEvent _ world = world

renderBlock :: Block -> Picture
renderBlock (BlockKey (xi,yi), BlockVal blockType) =
    color c $
    scale scaleFactor scaleFactor $
    translate x y $
    scale blockSize blockSize $ 
    Polygon [(-0.5,-0.5),(0.5,-0.5),(0.5,0.5),(-0.5,0.5)]
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

possibleLinks :: BlockKey -> [(BlockKey,LinkKey)]
possibleLinks (BlockKey (x,y)) = [(BlockKey (x+1,y),L2R (x  ,y  )),
                                  (BlockKey (x-1,y),L2R (x-1,y  )),
                                  (BlockKey (x,y-1),D2U (x  ,y-1)),
                                  (BlockKey (x,y+1),D2U (x  ,y  ))]

removeBlock :: BlockKey -> World -> World
removeBlock blockKey (World blocks links) =
    World (H.delete blockKey blocks) (foldl' (flip H.delete) links $ map snd $ possibleLinks blockKey)

addBlock :: Block -> World -> World
addBlock (key @(BlockKey (x,y)),val) (World blocks links) = World (H.insert key val blocks) $
    appEndo (foldMap addLink $ possibleLinks key) links
    where addLink (block,linkKey) = Endo $ if (block `H.member` blocks)
                                        then H.insert linkKey (LinkVal False)
                                        else id

roundToIntPoint :: Point -> IntPt
roundToIntPoint (x,y) = (round (x/scaleFactor), round (y/scaleFactor))

cycleBlockVal :: Maybe BlockVal -> Maybe BlockVal
cycleBlockVal Nothing                   = Just $ BlockVal Normal
cycleBlockVal (Just (BlockVal Normal))  = Just $ BlockVal Bedrock
cycleBlockVal (Just (BlockVal Bedrock)) = Nothing 