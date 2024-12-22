{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) William M. Farmer 2024
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2024
-}
module Assign_2 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Luca Mawyin
-- Date: October 27 2024
macid :: String
macid = "mawyinl"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description:
 -   returns the real portion of a gaussian number
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (a,b) = a

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description:
 -   Returns the imaginary portion of a gaussian number
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (a,b) = b

{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description:
 -   Returns the conjugate of a gaussian number
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj (a,b) = (a,-b)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description:
 -   Adds two gaussian number
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd x y = (a0+a1, b0+b1)
  where
    a0 = gaussReal x
    a1 = gaussReal y
    b0 = gaussImag x
    b1 = gaussImag y

{- -----------------------------------------------------------------
 - gaussMul
 - -----------------------------------------------------------------
 - Description:
 -   multiplies two gaussian numbers
 -}
gaussMul :: GaussianInt -> GaussianInt -> GaussianInt
gaussMul x y = (real, imag)
  where
    a0 = gaussReal x
    a1 = gaussReal y
    b0 = gaussImag x
    b1 = gaussImag y

    real = (a0*a1) - (b0*b1)
    imag = (a0*b1) + (a1*b0)

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description:
 -   Returns norm of a gaussian number
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm (a,b) = (a^2) + (b^2)

{- -----------------------------------------------------------------
 - gaussAddList
 - -----------------------------------------------------------------
 - Description:
 - Adds all the gaussian numbers in a list
 -}
gaussAddList :: [GaussianInt] -> GaussianInt
gaussAddList [] = (0,0)
gaussAddList (x:xs) = gaussAdd x (gaussAddList xs)

{- -----------------------------------------------------------------
 - gaussMulList
 - -----------------------------------------------------------------
 - Description:
 -   Multiplies gaussian ints in a list
 -}
gaussMulList :: [GaussianInt] -> GaussianInt
gaussMulList [] = (1,0)
gaussMulList (x:xs) = gaussMul x (gaussMulList xs)

{- ------------------------------------------------------------------------
 - gaussCircle
 - ------------------------------------------------------------------------
 - Description:
 -   Adds gauss numbers that have a norm less than n to a new list
 -}
gaussCircle :: [GaussianInt] -> Integer -> [GaussianInt]
gaussCircle [] _ = []
gaussCircle (x:xs) n

  -- When n is => 0, just return an empty list
  -- No possible case where a gaussInt norm can be equal to 0 and get added to array
    -- Will just result in an empty array anyways
    -- Just skipping that step for O(1) runtime over O(n)
  | n <= 0 = []

  -- norm of x is less than n
  | gaussNorm x < n = x : gaussCircle xs n

  -- norm of x is not less than n
  | otherwise = gaussCircle xs n


{-
TEST CASES
-}

{-
Function: gaussConj
Test Case Number: 1
Input: (2,3)
Expected Output: (2,-3)
Actual Output: (2,-3)
-}
gaussConjTest1 :: GaussianInt
gaussConjTest1 = gaussConj (2,3)
{-
Function: gaussConj
Test Case Number: 2
Input: (-1,2)
Expected Output: (-1,-2)
Actual Output: (-1,-2)
-}
gaussConjTest2 :: GaussianInt
gaussConjTest2 = gaussConj (-1,2)
{-
Function: gaussConj
Test Case Number: 3
Input: (3,-3)
Expected Output: (3,3)
Actual Output: (3,3)
-}
gaussConjTest3 :: GaussianInt
gaussConjTest3 = gaussConj (3,-3)



{-
Function: gaussAdd
Test Case Number: 1
Input: (3, 4) (-1, 2)
Expected Output: (2,6)
Actual Output: (2,6)
-}
gaussAddTest1 :: GaussianInt
gaussAddTest1 = gaussAdd (3, 4) (-1, 2)
{-
Function: gaussAdd
Test Case Number: 2
Input: (-8, 7) (3, -3)
Expected Output: (-5,4)
Actual Output: (-5,4)
-}
gaussAddTest2 :: GaussianInt
gaussAddTest2 = gaussAdd (-8, 7) (3, -3)
{-
Function: gaussAdd
Test Case Number: 3
Input: (6, -12) (-1, 2)
Expected Output: (5,-10)
Actual Output: (5,-10)
-}
gaussAddTest3 :: GaussianInt
gaussAddTest3 = gaussAdd (6, -12) (-1, 2)



{-
Function: gaussMul
Test Case Number: 1
Input: (3, 4) (-1, 2)
Expected Output: (-11,2)
Actual Output: (2,6)
-}
gaussMulTest1 :: GaussianInt
gaussMulTest1 = gaussMul (3, 4) (-1, 2)
{-
Function: gaussMul
Test Case Number: 2
Input: (-8, 7) (3, -3)
Expected Output: (-3,45)
Actual Output: (-3,45)
-}
gaussMulTest2 :: GaussianInt
gaussMulTest2 = gaussMul (-8, 7) (3, -3)
{-
Function: gaussMul
Test Case Number: 3
Input: (6, -12) (-1, 2)
Expected Output: (18,24)
Actual Output: (18,24)
-}
gaussMulTest3 :: GaussianInt
gaussMulTest3 = gaussMul (6, -12) (-1, 2)



{-
Function: gaussNorm
Test Case Number: 1
Input: (6, -12) 
Expected Output: 180
Actual Output: 180
-}
gaussNormTest1 :: Integer
gaussNormTest1 = gaussNorm (6, -12)
{-
Function: gaussNorm
Test Case Number: 2
Input: (-1, 2) 
Expected Output: 5
Actual Output: 5
-}
gaussNormTest2 :: Integer
gaussNormTest2 = gaussNorm (-1, 2)
{-
Function: gaussNorm
Test Case Number: 3
Input: (6, -12) 
Expected Output: 260
Actual Output: 260
-}
gaussNormTest3 :: Integer
gaussNormTest3 = gaussNorm (-14, -8)



{-
Function: gaussAddList
Test Case Number: 1
Input: [(1, 2), (3, 4), (5, 1), (0, 3), (-2, -1), (4, 0)]
Expected Output: (11,9)
Actual Output: (11,9)
-}
gaussAddListTest1 :: GaussianInt
gaussAddListTest1 = gaussAddList [(1, 2), (3, 4), (5, 1), (0, 3), (-2, -1), (4, 0)]
{-
Function: gaussAddList
Test Case Number: 2
Input: [(-2, 1), (5, 5), (3, -3), (4, 2), (0, 1), (2, 4), (-1, -1), (6, 3), (1, 0)]
Expected Output: (18,12)
Actual Output: (18,12)
-}
gaussAddListTest2 :: GaussianInt
gaussAddListTest2 = gaussAddList [(-2, 1), (5, 5), (3, -3), (4, 2), (0, 1), (2, 4), (-1, -1), (6, 3), (1, 0)]
{-
Function: gaussAddList
Test Case Number: 3
Input: [(1, 4), (-3, 2), (2, 2), (5, -5), (0, 3), (-2, 0), (4, 1)]
Expected Output: (7,7)
Actual Output: (7,7)
-}
gaussAddListTest3 :: GaussianInt
gaussAddListTest3 = gaussAddList [(1, 4), (-3, 2), (2, 2), (5, -5), (0, 3), (-2, 0), (4, 1)]



{-
Function: gaussMulList
Test Case Number: 1
Input: [(1, 2), (3, 4), (5, 1), (0, 3), (-2, -1), (4, 0)]
Expected Output: (660,1380)
Actual Output: (660,1380)
-}
gaussMulListTest1 :: GaussianInt
gaussMulListTest1 = gaussMulList [(1, 2), (3, 4), (5, 1), (0, 3), (-2, -1), (4, 0)]
{-
Function: gaussMulList
Test Case Number: 2
Input: [(-2, 1), (5, 5), (3, -3), (4, 2), (0, 1), (2, 4), (-1, -1), (6, 3), (1, 0)]
Expected Output: (-9000,-9000)
Actual Output: (-9000,-9000)
-}
gaussMulListTest2 :: GaussianInt
gaussMulListTest2 = gaussMulList [(-2, 1), (5, 5), (3, -3), (4, 2), (0, 1), (2, 4), (-1, -1), (6, 3), (1, 0)]
{-
Function: gaussMulList
Test Case Number: 3
Input: [(1, 4), (-3, 2), (2, 2), (5, -5), (0, 3), (-2, 0), (4, 1)]
Expected Output: (-6120,4080)
Actual Output: (-6120,4080)
-}
gaussMulListTest3 :: GaussianInt
gaussMulListTest3 = gaussMulList [(1, 4), (-3, 2), (2, 2), (5, -5), (0, 3), (-2, 0), (4, 1)]



{-
Function: gaussCircle
Test Case Number: 1
Input: [(1, 2), (3, 4), (5, 5), (2, 1), (4, 4), (0, 0)] 20
Expected Output: [(1,2),(2,1),(0,0)]
Actual Output: [(1,2),(2,1),(0,0)]
-}
gaussCircleTest1 :: [GaussianInt]
gaussCircleTest1 = gaussCircle x 20
  where
    x = [(1, 2), (3, 4), (5, 5), (2, 1), (4, 4), (0, 0)]
{-
Function: gaussCircle
Test Case Number: 2
Input: [(5, 3), (7, 6), (8, 7), (9, 3)] (-20)
Expected Output: []
Actual Output: []
-}
gaussCircleTest2 :: [GaussianInt]
gaussCircleTest2 = gaussCircle x (-20)
  where
    x = [(5, 3), (7, 6), (8, 7), (9, 3)]
{-
Function: gaussCircle
Test Case Number: 3
Input: [] 13
Expected Output: []
Actual Output: []
-}
gaussCircleTest3 :: [GaussianInt]
gaussCircleTest3 = gaussCircle [] 13