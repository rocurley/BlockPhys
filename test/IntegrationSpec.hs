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

import Debug.Trace

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

data Simulation = Simulation World [FakeInput] Time
instance Arbitrary Simulation where
  arbitrary = let
    initialWorld = addJail $ emptyWorld $ Player $  Grounded (SupPos (0,-3) 0) 0 Nothing
    in Simulation initialWorld <$> arbitrary <*> pure 0
  --The shrinks don't work fully because the trailing shrinks are needed to time evolve to the problem.
  --Add a time to the simulation type, where that time is evolved to after all the keypresses.
  --When shrinking from the right, add times to that time.
  --Also subject the end time to shrinking.
  shrink (Simulation world [] t) = Simulation world [] <$> shrink t
  shrink (Simulation world (i:is) t) =
    [ Simulation (stepInput i world) is t
    , let
        Just (inputs, FakeInput _ dt) = unsnoc $ i:is
        in Simulation world inputs $ t + dt
    ] ++ (Simulation world (i:is) <$> shrink t)
instance Show Simulation where
  show (Simulation world inputs t) = let
    worldStr = "addJail $ emptyWorld $ " ++ show (world^.player)
    in "Simulation (" ++ worldStr ++ ") " ++ show inputs ++ show t

stepInput :: FakeInput -> World -> World
stepInput (FakeInput event dt) = handleEvent event . stepWorld dt

playerInJail :: World -> Bool
playerInJail world = let
  (x,y) = world^.player.playerMovement.movLoc
  in (abs x < 3) && (abs y < 3)

spec :: Spec
spec = do
  describe "Collisions" $ do
    it "Players should stay in a box with random initial trajectory" $ property $ \ plr t -> let
      world = addJail $ emptyWorld plr
      in playerInJail $ stepWorld (abs t) world
    it "Players should stay in a box with button mashing" $ within (10^5) $ property $
      \ (Simulation world keypresses t)-> playerInJail world ==> let
      in playerInJail $ stepWorld t $ foldr stepInput world (keypresses :: [FakeInput])
    it "In this specific case, a collision should happen on the left side of the wall" $ let
      traj = PolyTrajectory (poly BE [7.5,0.0,2.3]) (poly BE [-2.1])
      dir = fmap (\ (Collision _ _ _ dir) -> dir) $ minimumByMay (comparing (\ (Collision t _ _ _) -> t)) $
        runReader (predictStaticCollisions  0.5 (SolidPlayer, traj)) $ addJail $ emptyWorld undefined
      in dir `shouldBe` Just LfDir

