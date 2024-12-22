{-|
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2022
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 1 - McMaster CS 1JC3 2024.

  Modified by W. M. Farmer 19-SEP-2024.
-}
module Assign_1 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS AND DO NOT ADD ANY IMPORTS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Luca Mawyin
-- Date: September 27, 2024

macid :: String
macid = "mawyinl"

(***) :: Double -> Double -> Double
x *** y = if x >= 0 then x ** y else -( (-x) ** y)

{- -----------------------------------------------------------------
 - qbrt
 - -----------------------------------------------------------------
 - Description:
 -   Gives the cube root of input x (double) as double
 -}
qbrt :: Double -> Double
qbrt x = x *** (1/3)

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description:
 -   Finds the Q value of the cubic function given a b c
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = numerator/denominator

  where
    numerator = (3*a*c) - (b**2)
    denominator = 9*(a**2)

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description:
 -   Finds the R value of cubic function given a b c d
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = (term1 - term2 - term3)/denominator

  where
    term1 = 9*a*b*c
    term2 = 27*(a**2)*d
    term3 = 2*(b**3)
    denominator = 54*(a**3)

{- -----------------------------------------------------------------
 - cubicDiscSign
 - -----------------------------------------------------------------
 - Description:
 -   Finds the value of the discriminant of Q and R
 -}
cubicDiscSign :: Double -> Double -> Int
cubicDiscSign q r
  | (q**3 + r**2) == 0 = 0
  | (q**3 + r**2) > 0 = 1
  | otherwise = -1


{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description:
 -   Finds the S value of cubic equation
 -}
cubicS :: Double -> Double -> Double
cubicS q r = qbrt (r + sqrt (q**3 + r**2))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description:
 -   Finds the T value of cubic equation
 -}
cubicT :: Double -> Double -> Double
cubicT q r = qbrt (r - sqrt (q**3 + r**2))

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description:
 -   Given the a b c d values, calculate roots
 -   Only calculates roots if discriminant is less than/equal to 0
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]

cubicRealSolutions a b c d

  -- Not a cubic equation
  | a == 0 = []

  -- roots require complex arithmetic
  | sign == -1  = []

  -- Roots exist
  | sign == 0 =

  -- Checking if any roots are equal
  -- Making sure to not print repeated roots

  -- if x1, x2, x3 are all the same
  if x1 === x2 && x1 === x3 then [x1]

  -- x1 == x2 || x2 == x3 -> x1 + x3
  else if (x1 === x2) || (x2 === x3) then [x1,x3]

  -- x1 == x3 || x2 == x3 -> x1 + x2
  else if (x1 === x3) || (x2 === x3) then [x1,x2]

  -- all roots are unique
  else [x1,x2,x3]

  -- Only 1 root
  | sign ==  1 = [x1]

  -- Safety net
  | otherwise   = []

  where
    sign = cubicDiscSign q r
    s    = cubicS q r
    t    = cubicT q r
    q    = cubicQ a b c
    r    = cubicR a b c d

    x1 = s + t - (b/(3*a))
    x2 = -((s+t)/2) - b/(3*a) -- Was told by friend that the (i*sqrt3/2)(s-t)
    x3 = -((s+t)/2) - b/(3*a) -- was not necessary




(===) :: Double -> Double -> Bool
x === y = let
  tol = 1e-3
  in abs (x-y) <= tol

{- -----------------------------------------------------------------
 - Test Cases

  ** USING THIS LINK TO VERIFY: https://calculator.academy/cardanos-formula-calculator/ **
 - -----------------------------------------------------------------
 -}

test1 :: [Double]
test1 = cubicRealSolutions 1 0 0 (-1)

test2 :: [Double]
test2 = cubicRealSolutions 1 2 1 0

test3 :: [Double]
test3 = cubicRealSolutions 1 0 (-3) 0

test4 :: [Double]
test4 = cubicRealSolutions 3 0 (-1) 0