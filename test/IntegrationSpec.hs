module IntegrationSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as Map

import Data.Foldable

import Control.Monad.State.Strict hiding (mapM,mapM_)
import Control.Monad.Reader hiding (mapM,mapM_)

import Data.Ord hiding (Down)
import Safe hiding (at)

import Control.Lens

import Physics
import Waldo
import World
import qualified Map2D

import Math.Polynomial hiding (x)

import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = hspec spec

data FakeInput = FakeInput{event :: Event, dt :: Time} deriving Show
instance Arbitrary FakeInput where
  arbitrary = do
    n <- choose(1,4)
    let key = SpecialKey $ case (n :: Int) of
          1 -> KeyDown
          2 -> KeyUp
          3 -> KeyRight
          4 -> KeyLeft
    stateBool <- arbitrary
    let keyState = if stateBool then Up else Down
    let event = EventKey key keyState (Modifiers Up Up Up) (0,0)
    dt <- abs <$> arbitrary
    return $ FakeInput event dt

stepInput :: FakeInput -> World -> World
stepInput (FakeInput event dt) = handleEvent event . stepWorld dt

spec :: Spec
spec = do
  describe "Collisions" $ do
    it "Players should stay in a box with random initial trajectory" $ within (10^5) $ property $ \ plr t -> let
      world = addJail $ emptyWorld plr
      newWorld = stepWorld (abs t) world
      (x,y) = newWorld^.player.playerMovement.movLoc
      in (abs x < 3) && (abs y < 3)
    it "Players should stay in a box with button mashing" $ within (10^5) $ property $ \ keypresses -> let
      world = addJail $ emptyWorld $ Player $  Grounded (SupPos (0,-3) 0) 0 Nothing
      newWorld = foldr stepInput world (keypresses :: [FakeInput])
      (x,y) = newWorld^.player.playerMovement.movLoc
      in (abs x < 3) && (abs y < 3)
    it "In this specific case, a collision should happen on the left side of the wall" $ let
      traj = PolyTrajectory (poly BE [7.5,0.0,2.3]) (poly BE [-2.1])
      dir = fmap (\ (Collision _ _ _ dir) -> dir) $ minimumByMay (comparing (\ (Collision t _ _ _) -> t)) $
        runReader (predictStaticCollisions  0.5 (playerShape, traj)) $ addJail $ emptyWorld undefined
      in dir `shouldBe` Just LfDir

