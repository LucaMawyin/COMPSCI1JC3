{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use list comprehension" #-}

{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) William M. Farmer 2024
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2024
-}
module Assign_3 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E., THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E., RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (E.G., IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Luca Mawyin
-- Date: November 17 2024
macid :: String
macid = "mawyinl"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}

data Poly a =
    X
  | Coef a
  | Sum (Poly a) (Poly a)
  | Prod (Poly a) (Poly a)
  deriving Show

newtype PolyList a = PolyList [a]
  deriving Show

instance Num a => Num (Poly a) where
  x + y         = Sum x y
  x * y         = Prod x y
  negate x      = (Coef (-1)) * x
  abs x         = error "abs is left un-implemented"
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - polyFun
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates Polynomial p at value c
 -    i.e. p(c)
 -}
polyFun :: Num a => Poly a -> a -> a
polyFun X c = c
polyFun (Coef a) c = a
polyFun (Prod a b) c = polyFun a c * polyFun b c
polyFun (Sum a b) c = polyFun a c + polyFun b c


{- -----------------------------------------------------------------
 - isZeroPoly - ADDED
 - -----------------------------------------------------------------
 - Description:
 -    Determines if a polynomial is a zero polynomial
 -    Used in polyDegree
 -}
isZeroPoly :: (Num a, Eq a) => Poly a -> Bool
isZeroPoly p =
  
  let

    -- Only taking list portion of PolyList
    isZeroPoly' (PolyList a) = null a

  -- If implemented properly polyToPolyList p should return [] for any zero poly representation
  in isZeroPoly' (polyToPolyList p)


{- -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - Description:
 -    Find degree of polynomial
 -}
polyDegree :: (Num a,Eq a) => Poly a -> Int

polyDegree p

  -- p is a Zero Poly
  | isZeroPoly p = undefined

  -- p is NOT a Zero Poly
  | otherwise =

    -- Need to have prime function so that we will not be led to zero poly
    let
      polyDegree' (Coef a) = 0
      polyDegree' X = 1
      polyDegree' (Sum a b) = max (polyDegree' a) (polyDegree' b)
      polyDegree' (Prod a b) = polyDegree' a + polyDegree' b
    in polyDegree' p


{- -----------------------------------------------------------------
 - polyListFun
 - -----------------------------------------------------------------
 - Description:
 -    Evaluate polynomial pl at c
 -    pl is a list of coefficients
 -    i.e. [1,2,3] = 1 + 2x + 3x^2
 -}
polyListFun :: Num a => PolyList a -> a -> a
polyListFun (PolyList []) _ = 0
polyListFun (PolyList (p:pl)) c = p + (c* (polyListFun (PolyList pl) c))


{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description:
 -    Checks degree of PolyList
 -}
polyListDegree :: (Num a,Eq a) => PolyList a -> Int

-- Removes trailing 0s before checking
polyListDegree (PolyList pl) = if null (listShrink pl) then undefined else length (listShrink pl) - 1

{- -----------------------------------------------------------------
 - listShrink - ADDED
 - -----------------------------------------------------------------
 - Description:
 -    removes trailing zeros in a list
 -    USED IN polyToPolyList AND polyListDegree
 -}
listShrink :: (Num a, Eq a) => [a] -> [a]
listShrink [] = []
listShrink [x] = if [x] == [0] then [] else [x]
listShrink xs = if last xs == 0 then listShrink (init xs) else (xs)


{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description:
 -    Add ql with pl
 -}

polyListSum :: Num a => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList a) (PolyList b) = PolyList (listSum a b)


{- -----------------------------------------------------------------
 - listSum - ADDED
 - -----------------------------------------------------------------
 - Description:
 -    Adds two lists together
 -    USED IN polyListSum AND polyToPolyList
 -    Avoids repetitive code
 -}
listSum :: Num a => [a] -> [a] -> [a]
listSum a b = 
  let 
    (matchedA, matchedB) = match a b
  in zipWith (+) matchedA matchedB

{- -----------------------------------------------------------------
 - zipOffset - ADDED
 - -----------------------------------------------------------------
 - Description:
 -    Offsets two lists then zips them together
 -    zipOffset [1,2,3,4] [5,6,7,8]
 -    [1,2,3,4] ++
 -    [0,5,6,7,8]
 -    = [1,7,9,11,8]
 -
 -    USED IN listProd
 -}
zipOffset :: Num a => [a] -> [a] -> [a]
zipOffset as bs =
  let
    zipOffset' as bs = listSum as bs
  in zipOffset' as (0 : bs)


{- -----------------------------------------------------------------
 - listProd - ADDED
 - -----------------------------------------------------------------
 - Description:
 -    Multiplies two polynomials together
 -    USED IN polyListProd AND polyToPolyList
 -    Avoids repetitive code
 -}
listProd :: Num a => [a] -> [a] -> [a]

-- Empty list means zero poly
listProd _ [] = []
listProd [] _ = []

-- Recursively multiply each element in ql by p
listProd [a] (b:bs) = a*b : listProd [a] bs

-- offset zipping the multiples of the lists
listProd (a:as) bs = zipOffset (listProd [a] bs) (listProd as bs)


{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description:
 -    Multiplies two polynomials together
 -}
polyListProd :: Num a => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList pl) (PolyList ql) = PolyList (listProd pl ql)


{- -----------------------------------------------------------------
 - polyExp - ADDED
 - -----------------------------------------------------------------
 - Description:
 -    Takes degree of x and returns poly representation
 -    USED IN polyListToPoly
 -}
polyExp :: Num a => Int -> Poly a
polyExp 0 = Coef 1
polyExp 1 = X
polyExp n = Prod X (polyExp (n-1))


{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description:
 -    Takes PolyList and converts to Poly
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly(PolyList []) = Coef 0
polyListToPoly pl =

  -- Taking PolyList item and degree (index)
  let
    polyListToPoly' (PolyList [p]) 0 = Coef p
    polyListToPoly' (PolyList [p]) d = Prod (Coef p) (polyExp d)
    polyListToPoly' (PolyList (p:pl)) d = Sum (polyListToPoly' (PolyList [p]) d) (polyListToPoly' (PolyList pl) (d+1))
  in polyListToPoly' pl 0


{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description:
 -    Convert a Poly to a PolyList
 -}
polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a

polyToPolyList p =

  -- Creating PolyList in terms of [Int] then casting it to PolyList
  let

    -- Base Cases
    polyToPolyList' (Coef a) = [a]
    polyToPolyList' X = [0,1]

    -- Sum of Poly
    polyToPolyList' (Sum a b) = listSum (polyToPolyList' a) (polyToPolyList' b)

    -- Prod of Poly
    polyToPolyList' (Prod a b) = listProd (polyToPolyList' a) (polyToPolyList' b)

  -- List gets shrunk before being returned
  in PolyList (listShrink (polyToPolyList' p))


{- -----------------------------------------------------------------
 - match - ADDED
 - -----------------------------------------------------------------
 - Description:
 -    Matches the length of the shortest list with length of longest list
 -    Returns a tuple with two lists that are the same length
 -    Ex. match [1,2,3,4] [1,2,3,4,5,6]
 -    = ([1,2,3,4,0,0],[1,2,3,4,5,6])
 -    
 -    USED IN polyToPolyList
 -}
match :: Num a => [a] -> [a] -> ([a],[a])
match a b
  | length a > length b = (b ++ (replicate (length a - length b) 0), a)
  | otherwise = (a ++ replicate (length b - length a) 0, b)


{-

---------------------------------------------------------- TEST PLAN ----------------------------------------------------------

------------ polyFun ------------

Function: polyFun
Test Case Number: 1
Input: (Coef 1) 2
Expected Output: 1
Actual Output: 1
Rationale: Base case to use as reference for Coef values

Function: polyFun
Test Case Number: 2
Input: (Sum (Sum (Prod X (Prod X X)) (Prod (Prod X (Coef 3)) X)) (Sum (Prod X (Coef 2)) (Coef 5))) 5
Expected Output: 215
Actual Output: 215
Rationale: Refer to test case 3

Function: polyFun
Test Case Number: 3
Input: (Sum (Coef 5) (Sum (Prod (Coef 2) X) (Sum (Prod (Coef 3) (Prod X X)) (Prod (Coef 1) (Prod X (Prod X X)))))) 5
Expected Output: 215
Actual Output: 215
Rationale: Seeing if different representations of the same polynomial are equated equivalently

------------ polyDegree ------------

Function: polyDegree
Test Case Number: 1
Input: Prod (Sum (Sum (Coef 2) X) (Sum X (Coef 0))) (Sum (Coef 2) (Coef (-2)))
Expected Output: undefined
Actual Output: undefined
Rationale: Seeing if complex representations of zero polynomials get the same treatment

Function: polyDegree
Test Case Number: 2 
Input: Sum (Sum (Prod X (Coef 0)) (Coef 4)) (Prod (Sum X X) X)
Expected Output: 2
Actual Output: 2
Rationale: Seeing if function can handle having zero polynomials in polynomial
           But polynomial is not zero polynomial

Function: polyDegree
Test Case Number: 3
Input: Prod (Sum (Prod (Prod (Sum X X) X) X) X) (Sum (Prod X X) X)
Expected Output: 5
Actual Output: 5
Rationale: Seeing what happens when we throw a bunch of X's at function

------------ polyListFun ------------

Function: polyListFun
Test Case Number: 1
Input: (PolyList [0,0,0,0]) 4
Expected Output: 0
Actual Output: 0
Rationale: Seeing how it handles some zero polyList

Function: polyListFun
Test Case Number: 2
Input: (PolyList [5,4,1,7,0]) 2
Expected Output: 73
Actual Output: 73
Rationale: Making sure it evaluates polyList properly even with a 0 present

Function: polyListFun
Test Case Number: 3
Input: (PolyList [5,(-4),1,7,(-6)]) 2
Expected Output: -39
Actual Output: -39
Rationale: Seeing how program reacts to negatives

------------ polyListDegree ------------

Function: polyListDegree
Test Case Number: 1
Input: PolyList [0]
Expected Output: undefined
Actual Output: undefined
Rationale: Refer to test case 2

Function: polyListDegree
Test Case Number: 2
Input: PolyList [0,0,0,0,0]
Expected Output: undefined
Actual Output: undefined
Rationale: Seeing if function is able to deal with different representations of zero polyList

Function: polyListDegree
Test Case Number: 3
Input: PolyList [1,3,2,0,0,4,0,0,0,0,0,0,0]
Expected Output: 5
Actual Output: 5
Rationale: Making sure function knows which 0s to not count

------------ polyListSum ------------

NOTE: FUNCTION IS NOT WRITTEN TO BE ABLE TO SHORTEN POLYLISTS
- WOULD REQUIRE EQ A AS PARAMETER BUT IT DOES NOT
- LEADING 0s dont skew the correctness but they could be removed if (Eq a) was included

Function: polyListSum
Test Case Number: 1
Input: (PolyList [1,3,2,0,0,4,0,0,0,0,0,0,0]) (PolyList [5,1,2,3])
Expected Output: PolyList [6,4,4,3,0,4,0,0,0,0,0,0,0] - FUNCTION IS NOT WRITTEN TO BE ABLE TO SHORTEN POLYLIST
Actual Output: PolyList [6,4,4,3,0,4,0,0,0,0,0,0,0]
Rationale: Making sure function can deal with different sized PolyList

Function: polyListSum
Test Case Number: 2
Input: (PolyList [1,5,2,(-2)]) (PolyList [5,1,2,3])
Expected Output: PolyList [6,6,4,1]
Actual Output: PolyList [6,6,4,1]
Rationale: Making sure function can deal with negatives

Function: polyListSum
Test Case Number: 3
Input: (PolyList [0,0,0]) (PolyList [5,8,25,2])
Expected Output: PolyList [5,8,25,2]
Actual Output: PolyList [5,8,25,2]
Rationale: Seeing how function deals with zero PolyLists

------------ polyListProd ------------

NOTE: SAME ISSUE AS POLYLISTSUM

Function: polyListProd
Test Case Number: 1
Input: (PolyList [0,0,0]) (PolyList [5,8,25,2])
Expected Output: PolyList [0,0,0,0,0] 
Actual Output: PolyList [0,0,0,0,0]
Rationale: Seeing how function deals with zero PolyLists

Function: polyListProd
Test Case Number: 2
Input: (PolyList [1,0,0,3]) (PolyList [3,0,0,0,4])
Expected Output: PolyList [3,0,0,9,4,0,0,12]
Actual Output: PolyList [3,0,0,9,4,0,0,12]
Rationale: Seeing how function deals with nonzero polynomials with zeros in them

Function: polyListProd
Test Case Number: 3
Input: (PolyList [3,0,0,0,4]) (PolyList [1,0,0,3])
Expected Output: PolyList [3,0,0,9,4,0,0,12]
Actual Output: PolyList [3,0,0,9,4,0,0,12]
Rationale: verifying commutability property

------------ polyListToPoly ------------

NOTE: SAME ISSUE
- ISSUE IS MORE PREVALENT DUE TO HOW BIG POLY BECOMES

Function: polyListToPoly
Test Case Number: 1
Input: (PolyList [3,0,0,0,4])
Expected Output: Sum (Coef 3) (Sum (Prod (Coef 0) X) (Sum (Prod (Coef 0) (Prod X X)) (Sum (Prod (Coef 0) (Prod X (Prod X X))) (Prod (Coef 4) (Prod X (Prod X (Prod X X)))))))
Actual Output: Sum (Coef 3) (Sum (Prod (Coef 0) X) (Sum (Prod (Coef 0) (Prod X X)) (Sum (Prod (Coef 0) (Prod X (Prod X X))) (Prod (Coef 4) (Prod X (Prod X (Prod X X)))))))
Rationale: Seeing how it handles 0s

Function: polyListToPoly
Test Case Number: 2
Input: (PolyList [(-3),4,8])
Expected Output: Sum (Coef (-3)) (Sum (Prod (Coef 4) X) (Prod (Coef 8) (Prod X X)))
Actual Output: Sum (Coef (-3)) (Sum (Prod (Coef 4) X) (Prod (Coef 8) (Prod X X)))
Rationale: Making sure negatives are good

Function: polyListToPoly
Test Case Number: 3
Input: (PolyList [0,0,0,0])
Expected Output: Sum (Coef 0) (Sum (Prod (Coef 0) X) (Sum (Prod (Coef 0) (Prod X X)) (Prod (Coef 0) (Prod X (Prod X X)))))
Actual Output: Sum (Coef 0) (Sum (Prod (Coef 0) X) (Sum (Prod (Coef 0) (Prod X X)) (Prod (Coef 0) (Prod X (Prod X X)))))
Rationale: Seeing how it handles zero polynomials

------------ polyToPolyList ------------

Function: polyToPolyList
Test Case Number: 1
Input: (Sum (Sum (Prod X (Prod X X)) (Prod (Prod X (Coef 3)) X)) (Sum (Prod X (Coef 2)) (Coef 5)))
Expected Output: PolyList [5,2,3,1]
Actual Output: PolyList [5,2,3,1]
Rationale: Refer to test case 2 

Function: polyToPolyList
Test Case Number: 2
Input: (Sum (Coef 5) (Sum (Prod (Coef 2) X) (Sum (Prod (Coef 3) (Prod X X)) (Prod (Coef 1) (Prod X (Prod X X))))))
Expected Output: PolyList [5,2,3,1]
Actual Output: PolyList [5,2,3,1]
Rationale: Making sure it gives same result for different representations of same polynomial

Function: polyToPolyList
Test Case Number: 3
Input: (Prod (Prod (Sum (Prod (Prod X X) X) (Prod X (Coef 2))) (Prod (Sum (Coef 2) (Prod X (Coef 2))) (Sum (Coef 2) (Coef (-8))))) X)
Expected Output: PolyList [0,0,-24,-24,-12,-12]
Actual Output: PolyList [0,0,-24,-24,-12,-12]
Rationale: Seeing how much of a curveball I can throw at this thing

Function: polyToPolyList
Test Case Number: 4
Input: (Sum (Coef 0) (Sum (Prod (Coef 0) X) (Sum (Prod (Coef 0) (Prod X X)) (Sum (Prod (Coef 0) (Prod X (Prod X X))) (Prod (Coef 0) (Prod X (Prod X (Prod X X))))))))
Expected Output: PolyList []
Actual Output: PolyList []
Rationale: Seeing if it does the zero poly thing properly

-}