module IntegrationSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Map as Map

import Data.Foldable

import Control.Monad.State.Strict hiding (mapM,mapM_)

import Control.Lens

import Physics
import Waldo
import World
import qualified Map2D

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Players" $ do
    it "should stay in a box" $ within (10^5) $ property $ \ plr t -> let
      addJail = execState $ do
        traverse_ cycleBlock [( x,-3) | x <- [-3..3]]
        traverse_ cycleBlock [( x, 3) | x <- [-3..3]]
        traverse_ cycleBlock [(-3, y) | y <- [-2..2]]
        traverse_ cycleBlock [( 3, y) | y <- [-2..2]]
      world = addJail $ World Map2D.empty Map.empty Map.empty [0..] plr
      newWorld = stepWorld (abs t) world
      (x,y) = newWorld^.player.playerMovement.playerLoc
      in (abs x < 3) && (abs y < 3)
