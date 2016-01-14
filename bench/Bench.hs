import Criterion.Main

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Lens

import Control.Monad.State.Strict hiding (mapM,mapM_)

import Data.Foldable

import Control.Monad.Reader

import Physics
import Waldo
import World

initialPlayer = Player $ Jumping (0.0,0.0) (7.4241276,2.2388887) (-13.159124)
addJail = execState $ do
  traverse_ cycleBlock [BlockKey ( x,-3) | x <- [-3..3]]
  traverse_ cycleBlock [BlockKey ( x, 3) | x <- [-3..3]]
  traverse_ cycleBlock [BlockKey (-3, y) | y <- [-2..2]]
  traverse_ cycleBlock [BlockKey ( 3, y) | y <- [-2..2]]
initialWorld = addJail $ World Map.empty Map.empty Map.empty [0..] initialPlayer

stepWorldBenchMark t = let
  newWorld = stepWorld t initialWorld
  in newWorld^.player.playerMovement.playerLoc -- poor man's deepseq

-- Our benchmark harness.
main = print $ runReader (predictCollision (initialPlayer^.playerMovement.to playerTrajectory) 20) initialWorld
--main = defaultMain [ bgroup "stepWorld" [ bench (show t)  $ nf stepWorldBenchMark t | t <- [1 .. 20]]]
