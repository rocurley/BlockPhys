{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase  #-}

import Prelude hiding (foldr,foldl,mapM,mapM_,sequence,concatMap)

import Graphics.Gloss.Interface.Pure.Game

import Control.Monad.State.Strict hiding (mapM,mapM_)
import Control.Monad.Reader hiding (mapM,mapM_)

import World
import Render
import Waldo

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
initialPlayer = Player $ Grounded (SupPos (-3, -3) 0) 0 Nothing
--initialPlayer = Player $ Jumping (0, (1 + playerHeight)/2) (1,0) 5
--initialPlayer = Player $ Falling (0.0,3.0) (1,1)
initialWorld :: World
initialWorld = execState (traverse cycleBlock [(x,-3)| x <- [-3..3]]) $ emptyWorld initialPlayer

