module PolynomialRootsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Safe.Foldable
import Data.Maybe

import Math.Polynomial
import Numeric.IEEE

import PolynomialRoot

main :: IO ()
main = hspec spec

spec = do
  describe "PolynomialRoot.findRoots" $ do
    it "should give the greatest point less than a root" $
      property $ within (10^6) $ \ as -> not (null as) ==> let
          p = poly BE (as :: [Float])
          roots = findRoots $ p
          in not (polyIsZero p) ==> fromMaybe True $ do
              minRoot <- minimumMay roots
              return $ case compare (evalPoly p minRoot) 0 of
                  EQ -> True
                  GT -> evalPoly p (succIEEE minRoot) < 0
                  LT -> evalPoly p (succIEEE minRoot) > 0
