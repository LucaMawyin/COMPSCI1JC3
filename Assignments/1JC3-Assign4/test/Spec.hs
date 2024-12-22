{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Data.List (nub,find,delete,intercalate,(\\))
import Data.Char (toLower)
import Test.QuickCheck

import Debug.Trace (trace)

import qualified Assign_4 as A4

data QuickProp = forall prop . Testable prop =>
                 QuickProp { quickPropName :: String
                           , quickPropMark :: Int
                           , quickPropFunc :: prop
                           }

runQuickCheckProp :: QuickProp -> IO ()
runQuickCheckProp (QuickProp n i p) = do
    putStrLn $ "Running Test: " ++ n
    result <- quickCheckWithResult stdArgs { maxSuccess = 10 } p
    case result of
        Success{} -> putStrLn $ n ++ " passed."
        Failure{} -> do
            putStrLn $ n ++ " failed."
            putStrLn $ "Failure: " ++ show result
        GaveUp{} -> putStrLn $ n ++ " gave up after reaching the maximum number of tests."
        
main :: IO ()
main = do
  if A4.macid == "TODO"
    then error "Please fill in your Mac ID field!"
    else putStrLn "**This test only checks for filled in Mac IDs.**"
