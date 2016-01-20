{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase  #-}

import Prelude hiding (foldr,foldl,mapM,mapM_,sequence,concatMap)

import Graphics.Gloss.Interface.Pure.Game

import Data.Fixed
import Data.Maybe

import Control.Monad.State.Strict hiding (mapM,mapM_)
import Control.Monad.Reader hiding (mapM,mapM_)

import qualified Data.Map as H

import Control.Lens

import World
import Render
import Waldo
import Utils
import qualified Map2D

main :: IO ()
main = play displayMode white 60
    initialWorld
    (runReader renderWorld)
    handleEvent
    stepWorld

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
initialPlayer = Player $ Jumping (0, (1 + playerHeight)/2) (1,0) 5
--initialPlayer = Player $ Falling (0.0,3.0) (1,1)
initialWorld :: World
initialWorld = execState (cycleBlock (0,0)) $ World Map2D.empty H.empty H.empty [0..] initialPlayer

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ pt) = execState $ do
    linkClicked <- asState $ linkClickCheck pt
    case linkClicked of
        Just linkKey -> toggleLink linkKey
        Nothing -> cycleBlock (roundToIntPoint pt)
handleEvent _ = id

linkTester :: Point -> LinkKey -> Reader World (Maybe LinkKey)
linkTester (x,y) linkKey = do
    linkVal <- view $ links.at linkKey
    return $ if inDiamond (x,y) && isJust linkVal
             then Just linkKey
             else Nothing

linkClickCheck :: Point -> Reader World (Maybe LinkKey)
linkClickCheck (x,y) = let
    (xi,xrem) = divMod' (x/scaleFactor) 1
    (yi,yrem) = divMod' (y/scaleFactor) 1
    u = xrem + yrem -1
    v = yrem - xrem
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
