{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Data.List (nub,find,delete,intercalate,(\\))
import Data.Char (toLower)
import Test.QuickCheck (quickCheck
                       ,quickCheckResult
                       ,quickCheckWithResult
                       ,stdArgs
                       ,maxSuccess
                       ,Result(Success)
                       ,within
                       ,Testable)

import qualified Assign_3 as A3

import Debug.Trace (trace)

-------------------------------------------------------------------------------------------
-- Usage:
-- 1. Copy this file over "app/Main.hs".
-- 2. Add "QuickCheck" to the "dependencies" list in "package.yaml", e.g.,
{-

dependencies:
- base >= 4.7 && < 5
- QuickCheck

-}
-- 3. Run "stack run" to see your results.
-------------------------------------------------------------------------------------------
-- * Configuration

-- | Default timeout for each quickcheck call in milliseconds
timeout = 30000000 -- 30 seconds

-- | List of quickcheck propositions for main to run
propList :: [QuickProp]
propList = [QuickProp "values"       2 valuesProp
           ,QuickProp "degrees"      2 degreesProp
           ,QuickProp "polyListSum"  2 polyListSumProp
           ,QuickProp "polyListProd" 2 polyListProdProp
           ,QuickProp "conversion"   2 conversionProp
           ]

{- -----------------------------------------------------------------
 -  QuickCheck Properties
 - -----------------------------------------------------------------
 -}

data QuickProp = forall prop . Testable prop =>
                 QuickProp { quickPropName :: String
                           , quickPropMark :: Int
                           , quickPropFunc :: prop
                           }

(===>) :: Bool -> Bool -> Bool
a ===> b = (not a) || b

instance Eq a => Eq (A3.PolyList a) where
  (A3.PolyList ps) == (A3.PolyList qs) = ps == qs

cleanse :: (Num a,Eq a) => [a] -> [a]
cleanse cs =
  case cs of
    (0:cs') -> cleanse cs'
    otherwise -> cs

listToPoly :: Num a => [a] -> A3.Poly a
listToPoly []     = A3.Coef 0
listToPoly [x]    = A3.Coef x
listToPoly (x:xs) = A3.Sum (A3.Coef x) (A3.Prod A3.X (listToPoly xs))

listValue :: Num a => [a] -> a -> a
listValue []     n = 0
listValue (c:cs) n = c + n * (listValue cs n)

-- | Correctly evaluates polynomials.
valuesProp :: [Int] -> Int -> Bool
valuesProp ps n =
  let
    p           = listToPoly ps
    pl          = A3.PolyList ps
    val         = listValue ps n
    polyVal     = A3.polyFun p n
    polyListVal = A3.polyListFun pl n
  in
    val == polyVal  && polyVal == polyListVal

-- | Correctly determines degrees.
degreesProp :: [Int] -> Bool
degreesProp ps =
  let
    ps'             = reverse (cleanse (reverse ps)) 
    pl              = A3.PolyList ps
    listDeg         = length ps' - 1
    polyListDeg     = A3.polyListDegree pl
    testPoly        = A3.Sum (A3.Coef 1) (A3.Prod (A3.Coef 0) A3.X)
    testPolyDeg     = A3.polyDegree testPoly
    testPolyList    = A3.PolyList [1,0]
    testPolyListDeg = A3.polyListDegree testPolyList
    preCond         = ps' /= []
  in
    preCond ===>
      (listDeg == polyListDeg) && (testPolyDeg == 0) && (testPolyListDeg == 0)

-- | Correctly adds two polynomials.
polyListSumProp :: [Int] -> [Int] -> Int -> Bool
polyListSumProp ps qs n =
  let
    pl     = A3.PolyList ps
    ql     = A3.PolyList qs
    pVal   = listValue ps n
    qVal   = listValue qs n
    sumVal = A3.polyListFun (A3.polyListSum pl ql) n
  in
    sumVal == pVal + qVal

-- | Correctly multiples two polynomials.
polyListProdProp :: [Int] -> [Int] -> Int -> Bool
polyListProdProp ps qs n =
  let
    pl      = A3.PolyList ps
    ql      = A3.PolyList qs
    pVal    = listValue ps n
    qVal    = listValue qs n
    prodVal = A3.polyListFun (A3.polyListProd pl ql) n
  in
    prodVal == pVal * qVal    

-- | "PolyList -> Poly -> PolyList" is identity.
conversionProp :: [Int] -> Bool
conversionProp ps =
  let
    pl  = A3.PolyList ps
    p   = A3.polyListToPoly pl
    pl' = A3.polyToPolyList p
    preCond = ps == [] || last ps /= 0
  in
    preCond ===> (pl == pl')

-- | Execute a QuickCheck proposition
runQuickCheckProp :: QuickProp -> IO Result
runQuickCheckProp (QuickProp n i p) = do putStrLn $ "Running Test: " ++ n
                                         quickCheckResult $ within timeout p

-- | Main Program
main :: IO ()
main = do 
          putStrLn $ "Running tests for macid: " ++ A3.macid
          mapM_ runQuickCheckProp propList
