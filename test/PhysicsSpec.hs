module PhysicsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Physics

main :: IO ()
main = hspec spec

isFinite :: RealFloat a => a -> Bool
isFinite = not . or . sequence [isNaN, isInfinite]

trajectoryIsFinite :: Trajectory -> Bool
trajectoryIsFinite (Parabola (x,y) (vx,vy) ay) = all isFinite [x,y,vx,vy,ay]
trajectoryIsFinite (RunTrajectory (x,y) vx ax vRunMax) = all isFinite [x,y,vx,ax,vRunMax]
trajectoryIsFinite (JumpTrajectory (x,y) (vx,vy) ay g jumpJerk) = all isFinite [x,y,vx,vy,ay,g,jumpJerk]

spec :: Spec
spec = do
  describe "Physics.naiveAtT" $ do
    it "should always give finite results" $
      property $ \ trajectory t -> trajectoryIsFinite $ naiveAtT trajectory t
  describe "Physics.atT" $ do
    it "should always give finite results" $
      property $ \ trajectory t -> trajectoryIsFinite $ atT trajectory t
  describe "Physics.criticalPoints" $ do
    it "should always give finite results" $
      property $ all isFinite . criticalPoints
  describe "Physics.trajectoryBox" $ do
    it "should always give finite results" $
      property $ \ trajectory t -> let
        ((xMin, yMin), (xMax, yMax)) = trajectoryBox trajectory (abs t)
        in all isFinite [xMin, yMin, xMax, yMax]
    it "should always contain a point within the time window" $
      property $ \ trajectory t1 t2 -> let
        tEval = min (abs t1) (abs t2)
        tMax = max (abs t1) (abs t2)
        ((xMin,xMax), (yMin,yMax)) = trajectoryBox trajectory tMax
        (x,y) = startPoint $ atT trajectory tEval
        in (xMin <= x) && (x <= xMax) && (yMin <= y) && (y <= yMax)
  --TODO: Add test that object is actually colliding when the collision is predicted.
