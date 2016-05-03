module PhysicsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Control.Monad.Reader

import Math.Polynomial

import Fuzzy

import Physics
import World

main :: IO ()
main = hspec spec

isFinite :: RealFloat a => a -> Bool
isFinite = not . or . sequence [isNaN, isInfinite]

trajectoryIsFinite :: Trajectory -> Bool
trajectoryIsFinite (PolyTrajectory px py) = all isFinite $ polyCoeffs LE px ++ polyCoeffs LE py

paritySwapX :: Trajectory -> Trajectory
paritySwapX (PolyTrajectory px py) = PolyTrajectory (negatePoly px) py

paritySwapY :: Trajectory -> Trajectory
paritySwapY (PolyTrajectory px py) = PolyTrajectory px (negatePoly py)

spec :: Spec
spec = do
  describe "Physics.atT" $ do
    it "should always give finite results" $
      property $ \ trajectory t -> trajectoryIsFinite $ atT t trajectory
    it "should be the identity when t=0" $
      property $ \ trajectory -> trajectory == atT 0 trajectory
    it "should be divisible" $
      property $ \ trajectory t1 t2 -> t1 >= 0 && t2 >= 0 ==>
        atT (t1+t2) trajectory ~=~ atT t2  (atT t1 trajectory)
    it "should be x-parity invariant" $
      property $ \ trajectory t -> t >= 0 ==>
        paritySwapX (atT t trajectory) == atT t (paritySwapX trajectory)
    it "should be y-parity invariant" $
      property $ \ trajectory t -> t >= 0 ==>
        paritySwapY (atT t trajectory) == atT t (paritySwapY trajectory)
    -- Add translational symmetry
  describe "Physics.xint" $ do
    it "should agree with atT about the collision location" $
      property $ \ y trajectory -> and [(xt ~=~ xc) && (yt ~=~ yc)
        | ((xc, yc), t) <- xint y trajectory, let (xt, yt) = startPoint (atT t trajectory)]
    --Add to x and y int t>0 and finiteness checks.
  describe "Physics.yint" $ do
    it "should agree with atT about the collision location" $
      property $ \ x trajectory -> and [pt == startPoint (atT t trajectory)| (pt, t) <- yint x trajectory]
  describe "Physics.criticalPoints" $ do
    it "should always give finite results" $
      within (10^5) $ property $ all isFinite . criticalPoints
  describe "Physics.trajectoryBox" $ do
    it "should always give finite results" $
      property $ \ trajectory t -> t >= 0 ==> let
        ((xMin, yMin), (xMax, yMax)) = trajectoryBox trajectory t
        in all isFinite [xMin, yMin, xMax, yMax]
    it "should always contain a point within the time window" $
      property $ \ trajectory tEval tMax -> tMax >= tEval && tEval >= 0 ==> let
        ((xMin,xMax), (yMin,yMax)) = trajectoryBox trajectory tMax
        (x,y) = startPoint $ atT tEval trajectory
        in (xMin <= x) && (x <= xMax) && (yMin <= y) && (y <= yMax)
  --TODO: Add test that object is actually colliding when the collision is predicted.
  --TODO: Add test that time evolving in 2 parts is the same as in 1 part.
  --TODO: Add parity symmetry test.
  describe "Physics.timeEvolvePlayerMovement" $ do
    it "should be divisible" $
      property $ \ mov t1 t2-> t1 >= 0 && t2 >= 0 ==>
        flip runReader (emptyWorld $ Player mov) $ do
            split <- timeEvolveMovement t2 =<< timeEvolveMovement t1 mov
            atOnce <- timeEvolveMovement (t1 + t2) mov
            return $ split ~=~ atOnce

