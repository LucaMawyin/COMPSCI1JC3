{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : 1JC3-Assign1.Spec
Copyright   :  (c) Curtis D'Alves 2022
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Assignment1 and a main function that runs each tests and prints the results.

  Modified by W. M. Farmer 19-SEP-2024.
-}
module Assignments.Spec where

import qualified Assign_1 as Student
import Data.Char
import Data.Fixed (mod')
import Test.QuickCheck (quickCheck
                       ,quickCheckResult
                       ,quickCheckWithResult
                       ,stdArgs
                       ,maxSuccess
                       ,Result(Success)
                       ,within
                       ,Testable)
import Test.Hspec
import Test.QuickCheck.Property (property)

-------------------------------------------------------------------------------------------
-- * QuickCheck Tests

-- | Existential type wrapper for QuickCheck propositions, allows @propList@ to essentially
--   act as a heterogeneous list that can hold any quickcheck propositions of any type
data QuickProp = forall prop . Testable prop =>
                 QuickProp { quickPropName :: String
                           , quickPropMark :: Int
                           , quickPropFunc :: prop
                           }

-- | Boolean implication
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y
infixr 4 ==>

{- Test cubicRealSolution
 - ----------------------------------------------------------------------------------------
 -    Polynomial roots should always be within a reasonable
 -    (albeit larger than should be acceptable) tolerance
 -    from 0 when evaluated
 -      Note: Straightforward implementation's of this method are susceptible to very large floating point
 -            error for "tricky" polynomials, and quickCheck is smart and will usually find them
 -            cubicRealProp constrains quickCheck to "easy" polynomials by limiting to integer coefficients
 -            between (-25,25)
 -}
cubicRealProp :: (Int,Int,Int,Int) -> Bool
cubicRealProp (a',b',c',d') = let
      (a,b,c,d) = (fromIntegral $ (a' `mod` 50) - 20
                  ,fromIntegral $ (b' `mod` 50) - 20
                  ,fromIntegral $ (c' `mod` 50) - 20
                  ,fromIntegral $ (d' `mod` 50) - 20)
      tol  = 8e-1
      sign = Student.cubicDiscSign (Student.cubicQ a b c) (Student.cubicR a b c d)
      xs   = Student.cubicRealSolutions a b c d
  in (abs a <= tol) || (case filter (not . isNaN) xs of
                          (x0:_) -> abs (hornerEval (a,b,c,d) x0) <= tol
                          []     -> sign == -1 || not (null xs))

{- Helper Function
 - ----------------------------------------------------------------------------------------
 - Evaluate given polynomial (a,b,c,d) at x0 using horner's method
 - https://en.wikipedia.org/wiki/Horner%27s_method
 -}
--hornerEval :: (Double,Double,Double,Double) -> Double -> Double
hornerEval (a,b,c,d) x0 = d + x0 * (c + x0 * (b + x0 * a))

-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "cubicRealSolutions" $ do
    it "should return approximately zero when outputs are evaluated in the polynomial"
      $ property $ cubicRealProp
