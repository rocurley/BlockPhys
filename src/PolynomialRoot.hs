module PolynomialRoot where

import Math.Polynomial
import Data.Ratio
import Data.Maybe
import Control.Monad
import Control.Applicative
import Numeric.IEEE
import Test.QuickCheck
import Safe.Foldable
import System.Timeout

import Debug.Trace

type SturmChain a = [Poly a]

sturmChain :: (Fractional a, Eq a) =>  Poly a -> SturmChain a
sturmChain p = sturmChain' (quotPoly p g) (quotPoly p' g) where
    p' = polyDeriv p
    g = gcdPoly p p'
    sturmChain' p0 p1
        |polyIsZero p1 = [p0]
        |otherwise = p0 : sturmChain' p1 (negatePoly $ remPoly p0 p1)

signsAt :: (Ord a, Num a) => SturmChain a -> a -> [Ordering]
signsAt chain x = [compare (evalPoly p x) 0 | p <- chain]

signChanges xs = let
    xsNoZeros = filter (/= EQ) xs
    in length $ filter id $ zipWith (/=) xsNoZeros $ tail xsNoZeros

nRootsInterval :: (Ord a, Fractional a) => Poly a -> a -> a -> Int
nRootsInterval p l r = let
    chain = sturmChain p
    in signChanges (signsAt chain l) - signChanges (signsAt chain r)

cauchyRootInterval :: (Ord a, Fractional a) => Poly a -> a
cauchyRootInterval p
    |polyDegree p == 0 = 1 -- This is a dummy result: There are no roots
cauchyRootInterval p = let
    an:as = polyCoeffs BE p
    in 1 + maximum (map abs as) / abs an

refineRootIntervals :: (Eq a, Fractional a) =>
    (a -> a -> Int) -> (a, a) -> [(a,a)]
refineRootIntervals rootCounter (l, r) =
    case rootCounter l r of
        0 -> empty
        1 -> return (l, r)
        _ -> do
            let mid = (l + r)/2
            (l',r') <- [(l,mid),(mid,r)]
            if l == mid || r == mid
            then return (l, r)
            else refineRootIntervals rootCounter (l', r')

isolateRoots :: (Ord a, Fractional a) => Poly a -> [(a,a)]
isolateRoots p = let
    m = cauchyRootInterval p
    in refineRootIntervals (nRootsInterval p) (-m,m)

average :: IEEE a => a -> a -> a
average x y =
    if signum x == signum y
    then x + (y-x)/2
    else (x + y)/2

bisectL :: (Show a, IEEE a) => (a -> a) -> a -> a -> a
bisectL f xLow xHigh
    | f xLow > f xHigh = bisectL f xHigh xLow
bisectL f xLow xHigh =
  let
    xLeft = min xLow xHigh
    xRight = max xLow xHigh
    in if succIEEE xLeft == xRight || xLeft == xRight
    then if f xRight == 0
         then xRight
         else xLeft
    else let
        xNew = (xLow + xHigh)/2
        yNew = f xNew
        in case compare yNew 0 of
            EQ -> xNew
            GT -> bisectL f xLow xNew
            LT -> bisectL f xNew xHigh

findRoots :: (Show a, IEEE a) => Poly a -> [a]
findRoots p
  |polyDegree p <= 0 = []
  |otherwise = do
    (l,r) <- isolateRoots p
    return $ bisectL (evalPoly p) l r

