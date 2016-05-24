module Fuzzy where

import World
import Physics

import Math.Polynomial

class FuzzyEq e where
  (~=~) :: e -> e -> Bool
  infix 4 ~=~

instance FuzzyEq Float where
  (~=~) 0 0 = True
  (~=~) x y = abs ((x-y)/(abs x + abs y)) < 10**(-3)

instance (FuzzyEq a, FuzzyEq b) => FuzzyEq (a,b) where
  (a,b) ~=~ (x,y) = a~=~x && b~=~y

instance FuzzyEq Trajectory where
  PolyTrajectory px1 py1 ~=~ PolyTrajectory px2 py2 =
    and (zipWith (~=~) (polyCoeffs LE px1) (polyCoeffs LE px2)) &&
      and (zipWith (~=~) (polyCoeffs LE py1) (polyCoeffs LE py2))

instance FuzzyEq SupPos where
  s1 ~=~ s2 = supPosPosition s1 ~=~ supPosPosition s2

instance FuzzyEq Movement where
  Falling pt1 v1 ~=~ Falling pt2 v2 = and [pt1 ~=~ pt2, v1 ~=~ v2]
  NewlyFalling pt1 v1 t1 ~=~ NewlyFalling pt2 v2 t2 = and [pt1 ~=~ pt2, v1 ~=~ v2, t1 ~=~ t2]
  Jumping pt1 v1 a1 ~=~ Jumping pt2 v2 a2 = and [pt1 ~=~ pt2, v1 ~=~ v2, a1 ~=~ a2]
  Grounded sup1 v1 dir1 ~=~ Grounded sup2 v2 dir2 = and [sup1 ~=~ sup2, v1 ~=~ v2, dir1 == dir2]
  _ ~=~ _ = False
instance FuzzyEq Collision where
  (Collision t1 (objA1, trajA1) (objB1, trajB1) dir1) ~=~
    (Collision t2 (objA2, trajA2) (objB2, trajB2) dir2)
    = (t1 ~=~ t2)
    && (objA1 == objA2)
    && (objB1 == objB2)
    && (trajA1 ~=~ trajA2)
    && (trajB1 ~=~ trajB2)
    && (dir1 == dir2)
