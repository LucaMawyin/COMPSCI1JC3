{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-|
Module      : HaskellExercises07.Exercises07
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 07 - McMaster CS 1JC3 2021
-}
module Exercises07 where

import Prelude hiding (curry,fmap)

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
macid = "mawyinl"

-- Exercise A
-----------------------------------------------------------------------------------------------------------
-- Implement the function iter that iterates the application of a function, i.e. iter n f creates a new
-- function, that calls f on its arguments n times
-- E.x.
--   iter 3 f == f . f . f
-----------------------------------------------------------------------------------------------------------
iter :: Int -> (a -> a) -> (a -> a)
iter 1 f = f
iter n f = iter (n-1) f . f

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement the function mapMaybe that works like map but over the Maybe a type instead of [a]
-----------------------------------------------------------------------------------------------------------
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just $ f x -- Evaluates everything on right of $


-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement the function concatMapMaybe that takes a function that returns a Maybe type, maps it over a
-- list, and keeps only the Just values (Hint: use concatMap and a function that converts Maybe a to [a])
-- E.x. concatMapMaybe (\x -> if x >= 0 then Just x else Nothing) [-1,0,2,-5]
--    == [0,2]
-----------------------------------------------------------------------------------------------------------
concatMapMaybe :: (a -> Maybe a) -> [a] -> [a]
concatMapMaybe f [] = []
concatMapMaybe f (x:xs) = maybeToList (f x) ++ concatMapMaybe f xs

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList (Nothing)  = []

-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the function curry that takes an uncurried function and converts it into a curried on
-- E.x. (curry fst) 0 1 == 0
-- NOTE use a lambda expression, and the implementation is VERY straight forward
-----------------------------------------------------------------------------------------------------------

-- Not using lambda expressions is arguably more straightforward what
curry :: ((a,b) -> c) -> a -> b -> c
curry f a b = f (a, b)

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement the function foldt on the following Tree data type, that folds a tree from the left-most tree
-- to the right
-----------------------------------------------------------------------------------------------------------
data Tree a = TNode a [Tree a]
  deriving (Show,Eq)

foldt :: (b -> a -> b) -> b -> Tree a -> b
foldt op v (TNode x ts) =
  let
    v' = foldl (foldt op) v ts
  in op v' x
     -- NOTE if you wrap your head around whats going on here, the sol'n is
     -- super simple

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement the function familyTree2Tree that converts the following FamilyTree data type to the
-- previously defined Tree data typed
-- NOTE you can avoid manually pattern matching on Maybe constructors by using a combination of mapMaybe
--      and converting Maybe a to [a]
-----------------------------------------------------------------------------------------------------------
data FamilyTree = Person { name   :: String
                         , mother :: Maybe FamilyTree
                         , father :: Maybe FamilyTree }
  deriving (Show,Eq)

familyTree2Tree :: FamilyTree -> Tree String
familyTree2Tree familyTree =
  let
    maybeMother = mother familyTree
    maybeFather = father familyTree
    t0 = maybeToList (mapMaybe (familyTree2Tree) maybeMother)
    t1 = maybeToList (mapMaybe (familyTree2Tree) maybeFather)
  in TNode (name familyTree) (t0 ++ t1)

-- Exercise G
-----------------------------------------------------------------------------------------------------------
-- Implement a function all family that takes a FamilyTree and returns a list of every person in the
-- family using foldt
-- NOTE first convert to a Tree, then use foldt. The only trick is figuring out what to use for op
-----------------------------------------------------------------------------------------------------------
allFamily :: FamilyTree -> [String]
allFamily  familyTree =
  let
    tree = familyTree2Tree familyTree
    op   = (\list x -> list ++ [x]) -- Concatinating our cumulative list with x
    v    = []
  in foldt op v tree
