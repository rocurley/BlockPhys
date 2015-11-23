module PhysicsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Physics

main :: IO ()
main = hspec spec

isFinite :: RealFloat a => a -> Bool
isFinite = not . or . sequence [isNaN, isInfinite]

spec :: Spec
spec = do
  describe "Physics.trajectoryBox" $ do
    it "should always give finite results" $
      property $ \ trajectory t -> let
        ((xMin, yMin), (xMax, yMax)) = trajectoryBox trajectory (abs t)
        in all isFinite [xMin, yMin, xMax, yMax]
    it "should always contain a point within the time window" $
      property $ \ trajectory t1 t2 -> let
        tEval = min (abs t1) (abs t2)
        tMax = max (abs t1) (abs t2)
        ((xMin,yMin), (xMax,yMax)) = trajectoryBox trajectory tMax
        (x,y) = startPoint $ atT trajectory tEval
        in (xMin <= x) && (x <= xMax) && (yMin <= y) && (y <= yMax)
  describe "Physics.criticalPoints" $ do
    it "should always give finite results" $
      property $ all isFinite . criticalPoints
