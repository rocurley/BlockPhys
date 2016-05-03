module UnitVectorSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import UnitVector
import Fuzzy

main :: IO ()
main = hspec spec

withUnitVector :: Testable prop => (UnitVector -> prop) -> Property
withUnitVector f =
  property $ \ ux uy -> (ux /= 0) || (uy /= 0) ==> let
    Just u = normalize (ux, uy)
    in f u

spec = do
  describe "UnitVector.project" $ do
    it "Should be idempotent" $
      withUnitVector $ \ u v -> project u v ~=~ project u (project u v)
  describe "UnitVector.antiproject" $ do
    it "Should be idempotent" $
      withUnitVector $ \ u v -> antiproject u v ~=~ antiproject u (antiproject u v)
