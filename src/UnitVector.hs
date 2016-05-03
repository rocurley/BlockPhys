module UnitVector
( UnitVector
, normalize
, project
, antiproject
) where

data UnitVector = UnitVector{x :: Float, y :: Float, lenSq :: Float} deriving (Show, Eq)

normalize :: (Float, Float) -> Maybe UnitVector
normalize (0,0) = Nothing
normalize (x,y) = let

  in Just $ UnitVector x y $ x^2 + y^2

project :: UnitVector -> (Float, Float) -> (Float, Float)
project (UnitVector ux uy l2) (vx, vy) = let
  dot = ux*vx + uy*vy
  in (ux*dot/l2, uy*dot/l2)

antiproject :: UnitVector -> (Float, Float) -> (Float, Float)
antiproject unit (vx,vy) = let
  (px, py) = project unit (vx, vy)
  in (vx - px, vy - py)
