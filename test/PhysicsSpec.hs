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

(~=~) :: Float -> Float -> Bool
(~=~) 0 0 = True
(~=~) x y = abs ((x-y)/(abs x + abs y)) < 10**(-3)

trajectoryFuzzyEquality :: Trajectory -> Trajectory -> Bool
trajectoryFuzzyEquality (Parabola (x1,y1) (vx1,vy1) ay1) (Parabola (x2,y2) (vx2,vy2) ay2) =
  and $ zipWith (~=~) [x1,y1,vx1,vy1,ay1] [x2,y2,vx2,vy2,ay2]
trajectoryFuzzyEquality (RunTrajectory  (x1,y1) vx1 ax1 vRunMax1)(RunTrajectory  (x2,y2) vx2 ax2 vRunMax2) =
  and $ zipWith (~=~) [x1,y1,vx1,ax1,vRunMax1] [x2,y2,vx2,ax2,vRunMax2]
trajectoryFuzzyEquality
  (JumpTrajectory (x1,y1) (vx1,vy1) ay1 g1 jumpJerk1)
  (JumpTrajectory (x2,y2) (vx2,vy2) ay2 g2 jumpJerk2) = and $ zipWith (~=~)
    [x1,y1,vx1,vy1,ay1,g1,jumpJerk1] [x2,y2,vx2,vy2,ay2,g2,jumpJerk2]
trajectoryFuzzyEquality _ _ = False

paritySwapX :: Trajectory -> Trajectory
paritySwapX (Parabola (x,y) (vx,vy) ay) = Parabola (-x,y) (-vx,vy) ay
paritySwapX (RunTrajectory  (x1,y1) vx1 ax1 vRunMax1) =
    RunTrajectory  (-x1,y1) (-vx1) (-ax1) vRunMax1
paritySwapX (JumpTrajectory (x1,y1) (vx1,vy1) ay1 g1 jumpJerk1) =
    (JumpTrajectory (-x1,y1) (-vx1,vy1) ay1 g1 jumpJerk1)

paritySwapY :: Trajectory -> Trajectory
paritySwapY (Parabola (x,y) (vx,vy) ay) = Parabola (x,-y) (vx,-vy) (-ay)
paritySwapY (RunTrajectory  (x1,y1) vx1 ax1 vRunMax1) =
    RunTrajectory  (x1,-y1) vx1 ax1 vRunMax1
paritySwapY (JumpTrajectory (x1,y1) (vx1,vy1) ay1 g1 jumpJerk1) =
    (JumpTrajectory (x1,-y1) (vx1,-vy1) (-ay1) (-g1) (-jumpJerk1))

spec :: Spec
spec = do
  describe "Physics.naiveAtT" $ do
    it "should always give finite results" $
      property $ \ trajectory t -> trajectoryIsFinite $ naiveAtT trajectory t
    it "should be the identity when t=0" $
      property $ \ trajectory -> trajectory == naiveAtT trajectory 0
    it "should be divisible" $
      property $ \ trajectory t1 t2 -> t1 >= 0 && t2 >= 0 ==>
        trajectoryFuzzyEquality (naiveAtT trajectory (t1+t2)) (naiveAtT (naiveAtT trajectory t1) t2)
    it "should be x-parity invariant" $
      property $ \ trajectory t -> t >= 0 ==>
        paritySwapX (naiveAtT trajectory t) == naiveAtT (paritySwapX trajectory) t
    it "should be y-parity invariant" $
      property $ \ trajectory t -> t >= 0 ==>
        paritySwapY (naiveAtT trajectory t) == naiveAtT (paritySwapY trajectory) t
  describe "Physics.atT" $ do
    it "should always give finite results" $
      property $ \ trajectory t -> trajectoryIsFinite $ atT trajectory t
    it "should not modify the start location when t=0" $
      property $ \ trajectory -> startPoint (atT trajectory 0) == startPoint trajectory
    it "should be the idempotent when t=0" $
      property $ \ trajectory -> atT trajectory 0 == atT (atT trajectory 0) 0
    it "should be divisible" $
      property $ \ trajectory t1 t2 -> t1 >= 0 && t2 >= 0 ==>
        trajectoryFuzzyEquality (atT trajectory (t1+t2)) (atT (atT trajectory t1) t2)
    it "should be x-parity invariant" $
      property $ \ trajectory t -> t >= 0 ==>
        paritySwapX (atT trajectory t) == atT (paritySwapX trajectory) t
    it "should be y-parity invariant" $
      property $ \ trajectory t -> t >= 0 ==>
        paritySwapY (atT trajectory t) == atT (paritySwapY trajectory) t
    -- Add translational symmetry
  describe "Physics.xint" $ do
    it "should agree with atT about the collision location" $
      property $ \ y trajectory -> and [pt == startPoint (atT trajectory t)| (pt, t) <- xint y trajectory, t>=0]
  describe "Physics.yint" $ do
    it "should agree with atT about the collision location" $
      property $ \ x trajectory -> and [pt == startPoint (atT trajectory t)| (pt, t) <- yint x trajectory, t>=0]
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
        (x,y) = startPoint $ atT trajectory tEval
        in (xMin <= x) && (x <= xMax) && (yMin <= y) && (y <= yMax)
  --TODO: Add test that object is actually colliding when the collision is predicted.
  --TODO: Add test that time evolving in 2 parts is the same as in 1 part.
  --TODO: Add parity symmetry test.
