{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant if" #-}
{-|
Module      : 1JC3-Assign4.Assign_4.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 4 - McMaster CS 1JC3 2021
-}
module Assign_4 where
import Test.QuickCheck

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
-- Date: December 12 2024
macid :: String
macid = "mawyinl"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Add (MathExpr a) (MathExpr a)
  | Mult (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
  deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -    Evaluates expression at v
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval (Coef c) v     = c
eval X v            = v
eval (Add a b) v    = (eval a v) + (eval b v)
eval (Mult a b) v   = (eval a v) * (eval b v)
eval (Power a b) v  = (eval a v) ^^ b
eval (Cos a) v      = cos (eval a v)
eval (Sin a) v      = sin (eval a v)
eval (Abs a) v      = abs (eval a v)

{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Creating symbol notation for MathExpr
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Add x y
  x * y         = Mult x y
  negate x      = (Coef (-1)) * x
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Creating Fractional notation for expressions
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e (-1)
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    Convert a value to instance of MathExpr
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin     = Sin
  cos     = Cos
  -- log is actually ln
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp     = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -    Differentiates given MathExpr
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a

-- Simplify before and after
-- Otherwise things such as exponents end up getting expanded and then differentiated
diff e = simplify $ case e of

  -- Constant rule
  Coef a -> 0

  -- Variable rule
  X -> Coef 1

  -- Sum rule
  Add a b -> diff a + diff b

  -- Product rule
  Mult a b -> case (a, b) of

    -- Multiplying function by a constant
    (Coef c, y) -> Coef c * diff y
    (x, Coef c) -> Coef c * diff x

    -- Otherwise
    _ -> (b * diff a) + (a * diff b)

  -- Power rule
  Power a b -> (fromIntegral b) * (diff a) * (Power a (b-1))

  -- Cosine rule
  Cos a -> negate ((diff a) * (Sin a))

  -- Sine rule
  Sin a -> (diff a) * (Cos a)

  -- Absolute value rule
  Abs a -> a * (diff a) * (recip (Abs a))

{- -----------------------------------------------------------------
 - simplify
 - -----------------------------------------------------------------
 - Description:
 -    Simplifies given MathExpr
 -    Also formats MathExpr
 -      Ex. X*3*(Cos X) -> 3*X*(Cos X)
 -    I am a god in human clothes
 -}
simplify :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simplify expr = case expr of

  -- Addition rules
  Add a b -> case (simplify a, simplify b) of 

    -- n + 0 = n
    (Coef 0, u) -> u
    (u, Coef 0) -> u

    -- Combining coefficients
    (Coef n, Coef m) -> Coef (n + m)

    -- Commutability  
    (Add u v, w)
      -- Combining like terms
      | commute w u -> simplify ((2 * u) + v)
      | commute v w -> simplify ((2 * v) + u)   
      -- Checks if it's worth commuting
      | isSimpler ((simplify u) + simplify (v + w)) ((u + v) + w) -> simplify ((simplify u) + simplify (v + w))
      | isSimpler ((simplify v) + simplify (u + w)) ((u + v) + w) -> simplify ((simplify v) + simplify (u + w))
      -- Not worth commuting
      | otherwise -> w + (u + v)

    (u, Add v w)
      -- Combining like terms
      | commute v u -> simplify ((2 * u) + w)
      | commute u w -> simplify ((2 * u) + v)
      -- Checks if it's worth commuting
      | isSimpler ((simplify v) + simplify (u + w)) (u + (v + w)) -> simplify ((simplify v) + simplify (u + w))
      | isSimpler ((simplify w) + simplify (u + v)) (u + (v + w)) -> simplify ((simplify w) + simplify (u + v))
      -- Not worth commuting
      | otherwise -> u + (v + w)

    -- Combining like terms
    (Mult u v, Mult w x)
      | commute u w -> simplify (u * (v + x))
      | commute u x -> simplify (u * (v + w))
      | commute v w -> simplify (v * (u + x))
      | commute v x -> simplify (v * (u + w))
    (u, Mult v w) 
      | commute u v -> simplify (v * (w + 1))
      | commute u w -> simplify (w * (v + 1))
    (Mult u v, w)
      | commute u w -> simplify (u * (v + 1))
      | commute v w -> simplify (v * (u + 1))

    -- Base Case
    (u, v) -> if commute u v then simplify (2 * u) else u + v

  -- Multiplication rules
  Mult a b -> case (simplify a, simplify b) of 

    -- n * 0 = 0
    (Coef 0, u) -> Coef 0
    (u, Coef 0) -> Coef 0

    -- n * 1 = n
    (Coef 1, u) -> u
    (u, Coef 1) -> u

    -- Comining coefficients
    (Coef u, Coef v) -> Coef (u*v)

    -- Making sure coefficient is leading the expression
    -- Not necessary but it makes formatting nicer
    (u, Coef n) -> (Coef n) * u
    
    -- Multiplying with powers
    (Power u n, Power v m) | commute u v -> simplify (Power u (n + m))
    (u, Power v n) | commute u v -> simplify (Power v (n + 1))
    (Power u n, v) | commute u v -> simplify (Power u (n + 1))

    -- Commutability 
    (Mult u v, w)
      -- Combining like terms
      | commute u w -> simplify (v * (Power w 2))
      | commute v w -> simplify (u * (Power w 2))
      -- Checks if it's worth commuting
      | isSimpler ((simplify u) * simplify (v * w)) (w * (u * v)) -> simplify ((simplify u) * simplify (v * w))
      | isSimpler ((simplify v) * simplify (u * w)) (w * (u * v)) -> simplify ((simplify v) * simplify (u * w))
      -- Not worth commuting
      -- Prioritizing Coef in front
      -- Prioritizing X trailing Coef
        -- If there's no Coef in front then wtv
      | otherwise -> case (w, u, v) of
        (_, Coef _, _) -> u * getX (v * w)
        (_, _, Coef _) -> v * getX (w * u)
        _              -> w * getX (v * u)

    (u, Mult v w)
      -- Combining like terms
      | commute u v -> simplify (w * (Power u 2))
      | commute u w -> simplify (v * (Power u 2))
      -- Checks if it's worth commuting
      | isSimpler ((simplify v) * simplify (u * w)) (u * (v * w)) -> simplify ((simplify v) * simplify (u * w))
      | isSimpler ((simplify w) * simplify (u * v)) (u * (v * w)) -> simplify ((simplify w) * simplify (u * v))
      -- Not worth commuting
      -- Prioritizing Coef in front
      -- Prioritizing X trailing Coef
        -- If there's no Coef in front then wtv
      | otherwise -> case (u, v, w) of 
        (_, Coef _, _) -> v * getX (w * u)
        (_, _, Coef _) -> w * getX (v * u)
        _              -> u * getX (w * v)

    -- Distributive property
    -- If the expanded version is simpler then we expand
    (u, Add v w) | isSimpler (simplify (u * v) + simplify (u * w)) (u * (v + w)) -> simplify (u * v) + simplify (u * w)
    (Add u v, w) | isSimpler (simplify (w * u) + simplify (w * v)) (w * (u + v)) -> simplify (w * u) + simplify (w * v)

    -- Base Case
    (u, v) -> if commute u v then simplify (Power u 2) else u * v

  -- Power rules
  Power a b -> case (simplify a, b) of

    -- n ^ 0 = 1
    (_, 0) -> Coef 1

    -- n ^ 1 = n
    (u, 1) -> u

    -- Can just be equated to some number
    (Coef u, v) -> Coef (u^^v)

    -- Power to a power
    (Power u n, m) -> Power u (n*m)

    -- Base case
    (u, v) -> Power u v

  -- Absolute value rules
  Abs a -> case (simplify a) of 

    -- Nested abs
    (Abs u) -> abs (simplify u)

    -- Just the absolute value of some number
    (Coef u) -> Coef (abs u)

    -- Base case
    u -> abs u

  -- Sine/Cosine simplification
  Cos a -> Cos (simplify a)
  Sin a -> Sin (simplify a)

  -- Function is the most simple it can be
  _ -> expr

{- -----------------------------------------------------------------
 - commute - ADDED
 - -----------------------------------------------------------------
 - Description:
 -     Checks if two expressions are commutive versions of each other
 -     Only really needed for addition/multiplication
 -}
commute :: Eq a => MathExpr a -> MathExpr a -> Bool
commute a b = case (a, b) of

  -- Simple expressions
  (Coef u, Coef v) -> u == v
  (X, X) -> True

  -- Addition
  (Add (Add u v) w, Add (Add x y) z) -> 
    (commute (Add u v) (Add x y) && commute w z) || (commute (Add u v) (Add z y) && commute w x)
  
  -- Addition base case
  (Add u v, Add w x) -> (commute u x && commute v w) || (commute v x && commute u w)

  -- Multiplication
  (Mult (Mult u v) w, Mult (Mult x y) z) -> 
      (commute (Mult u v) (Mult x y) && commute w z) || (commute (Mult u v) (Mult z y) && commute w x)

  -- Multiplication base case
  (Mult u v, Mult x y) -> 
      (commute u x && commute v y) || (commute u y && commute v x)

  -- Power commutability
  (Power u n, Power v m) -> commute u v && n == m

  -- Sine/Cosine expressions
  (Cos u, Cos v) -> commute u v
  (Sin u, Sin v) -> commute u v

  -- Absolute Value
  (Abs u, Abs v) -> commute u v

  -- Base case
  _ -> False

{- -----------------------------------------------------------------
 - complexity - ADDED
 - -----------------------------------------------------------------
 - Description:
 -     Checks complexity of an expression
 -     Uses a scoring system to determine complexity of expression
 -     Used to simplify expressions
 -}
complexity :: MathExpr a -> Int
complexity u = case u of
  X             -> 1
  Coef _        -> 2
  (Add u v)     -> 2 + complexity u + complexity v
  (Mult u v)    -> 5 + complexity u + complexity v
  (Power u n)   -> 3 + complexity u + abs n
  (Cos u)       -> 3 + complexity u
  (Sin u)       -> 3 + complexity u
  (Abs u)       -> 2 + complexity u

{- -----------------------------------------------------------------
 - isSimpler - ADDED
 - -----------------------------------------------------------------
 - Description:
 -     checks which expression has better complexity
 -}
isSimpler :: MathExpr a -> MathExpr a -> Bool
isSimpler u v = complexity u < complexity v

{- -----------------------------------------------------------------
 - getX - ADDED
 - -----------------------------------------------------------------
 - Description:
 -    Returns MathExpr with X leading
 -}
getX :: (Eq a, Floating a) => MathExpr a -> MathExpr a
getX e = case e of
  (Add u v) -> if v == X then v + u else e

  -- Leading coefficient case doesn't really need to exist
  -- Because simplify already makes coefficient lead
  -- But this is incase someone uses this function in isolation
  (Mult a X) -> case a of 
    (Coef _) -> a*X  
    _        -> X*a
  (Mult X b) -> case b of
    (Coef _) -> b*X
    _        -> X*b 

  (Mult a (Power X n)) -> (Power X n) * a
  (Mult a b) -> getX a * getX b

  -- Base case
  _ -> e

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
 - Formats MathExpr to make it more legible
 - I'm willing to lose marks for not formatting exactly to requirements if it means that output is more legible
 -}
-- NOTE: you'll have to test pretty yourself
pretty :: (Show a) => MathExpr a -> String
pretty e = case e of
  X         -> "X"
  Coef a    -> show a
  Add a b   -> "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
  Mult a b  -> "(" ++ pretty a ++"*"++ pretty b ++ ")"
  Power a b ->  "(" ++ pretty a ++ "^^" ++ show b ++")"
  Cos a     -> "(Cos " ++ pretty a ++ ")"
  Sin a     -> "(Sin " ++ pretty a ++ ")"
  Abs a     -> "(Abs " ++ pretty a ++ ")"


---------------------------------------------------------- QUICKCHECKS ----------------------------------------------------------

infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- -----------------------------------------------------------------
 - expressionDepth & expressionDepthDivisor
 - -----------------------------------------------------------------
 - Variable to modify how long a given MathExpr is in quickCheck properties
 - Recommendations are based on my PC (CPU: i5 13600kf RAM: 48gb 3200MHz)
 -    Possibly subject to change based on user PC specs
 -    Feel free to modify depth
 -
 - RECOMMENDED:
 -    expressionDepth 5 to 10
 -    expressionDepthDivisor 1 to 3
 -}
expressionDepth :: Int 
expressionDepth = 10

expressionDepthDivisor :: Int
expressionDepthDivisor = 1

-- Creating an instance for quickCheck to generate MathExpr
instance Arbitrary a => Arbitrary (MathExpr a) where
  arbitrary = sized $ \n -> 
    oneof [
      return X,
      Coef <$> arbitrary,
      resize (n `div` 2) $ Add <$> arbitrary <*> arbitrary,
      resize (n `div` 2) $ Mult <$> arbitrary <*> arbitrary,
      resize (n `div` 2) $ Power <$> arbitrary <*> arbitrary,
      resize (n `div` 2) $ Cos <$> arbitrary,
      resize (n `div` 2) $ Sin <$> arbitrary,
      resize (n `div` 2) $ Abs <$> arbitrary
    ]


---------------------------------------------------------- diffProp quickCheck ----------------------------------------------------------

{-
 - Function: runDiffProp
 - Property: eval of derivative of a at 0 =~ expectedEval of expected derivative of a at 0 for all MathExpr a at 0
 -    A lot quicker if we just default to eval (diff a) 0 instead of randomizing point
 -    Also reduces chance of running into rounding errors from floating points
 - Actual Test Result: Pass
 -}
runDiffProp :: IO ()
runDiffProp = quickCheck diffProp

diffProp :: MathExpr Float -> Bool
diffProp e = 
  let 
    limitedExpr = limitExpressionDepth e expressionDepth
    a = eval (diff limitedExpr) 0
    b = expectedEval (expectedDiff limitedExpr) 0
  in
    (isNaN a && isNaN b) || 
    (isInfinite a && isInfinite b) ||
    (a =~ b)

-- Limiting how long out expression is
-- Quickcheck basically generates a long MathExpr and we only take the first 10 elements
-- Or else evaluating the expression takes WAY too long
limitExpressionDepth :: MathExpr Float -> Int -> MathExpr Float
limitExpressionDepth e 0 = Coef 0
limitExpressionDepth X _ = X
limitExpressionDepth (Coef x) _ = Coef x
limitExpressionDepth (Add a b) n = Add (limitExpressionDepth a (n-1)) (limitExpressionDepth b (n-1))
limitExpressionDepth (Mult a b) n = Mult (limitExpressionDepth a (n-1)) (limitExpressionDepth b (n-1))
limitExpressionDepth (Power a b) n = Power (limitExpressionDepth a (n-1)) b
limitExpressionDepth (Sin a) n = Sin (limitExpressionDepth a (n-1))
limitExpressionDepth (Cos a) n = Cos (limitExpressionDepth a (n-1))
limitExpressionDepth (Abs a) n = Abs (limitExpressionDepth a (n-1))

-- Just the diff that we are expecting from an inputted MathExpr
expectedDiff :: MathExpr Float -> MathExpr Float
expectedDiff e = simplify $ case e of
  Coef _ -> Coef 0
  X -> 1
  Add a b -> expectedDiff a + expectedDiff b
  Mult a b -> (b * expectedDiff a) + (a * expectedDiff b)
  Power b n -> (fromIntegral n) * (expectedDiff b) * (Power b (n-1))
  Sin a -> (expectedDiff a) * (Cos a)
  Cos a -> (-1) * (expectedDiff a) * (Sin a)
  Abs a -> a * (Power (Abs a) (-1)) * (expectedDiff a)

---------------------------------------------------------- evalProp quickCheck ----------------------------------------------------------

{-
 - Function: runEvalProp
 - Property: eval a v == expectedEval a v for all MathExpr a at all values v
 - Actual Test Result: Pass
 -}
runEvalProp :: IO ()
runEvalProp = quickCheck evalProp

evalProp :: MathExpr Float -> Float -> Bool
evalProp expr v = 
  let 

    -- Really gotta limit length
    -- Really easy for expressions to become way too long
    limitedExpr = limitExpressionDepth expr (expressionDepth `div` expressionDepthDivisor)

    -- expr has to be simplified or else the rounding error becomes WAY to large
    a = (eval (simplify expr) v)
    b = (expectedEval expr v)

  in
    (isNaN a && isNaN b) || 
    (isInfinite a && isInfinite b) ||
    (a =~ b)

-- Just the eval that we are expecting from inputted MathExpr
expectedEval :: MathExpr Float -> Float -> Float
expectedEval e v = case (simplify e) of 
  Coef c -> c
  Power b n -> (expectedEval b v) ^^ n
  Mult a b -> (expectedEval a v) * (expectedEval b v)
  Add a b -> (expectedEval a v) + (expectedEval b v)
  Abs a -> abs (expectedEval a v)
  Sin a -> sin (expectedEval a v)
  Cos a -> cos (expectedEval a v)

  -- Base case 
  _ -> v


{-  

---------------------------------------------------------- TEST PLAN ----------------------------------------------------------

------------ eval ------------

Function: eval
Test Case Number: 1
Input: ((1/0)*(Sin X)*(X^^2)) 0
Expected Output: NaN
Actual Output: NaN
Rationale: See how haskell evaluates Infinity * 0 -> NaN is correct

Function: eval
Test Case Number: 2
Input: ((sin X)^2 + (cos X)^2) 5
Expected Output: 1.0
Actual Output: 1.0
Rationale: Refer to test 3

Function: eval
Test Case Number: 3
Input: ((sin X)^2 + (cos X)^2) 13
Expected Output: 1.0
Actual Output: 1.0
Rationale: Seeing if identities like (Sin X)^2 + (Cos X)^2 = 1 hold

------------ diff ------------

** OUTPUTS ARE REPRESENTED AS IF THEY WERE RUN THROUGH PRETTY FUNCTION **
** OTHERWISE OUTPUTS BECOME VERY LONG **

NOTE: Expected is not 1:1 with actual output due to commutation of addition and multiplication

Function: diff
Test Case Number: 1
Input: (X^2)*3 + ((3*5)*X) + (5*3)
Expected Output: (6*X) + 15
Actual Output: ((6.0*X) + 15.0)
Rationale: Laying a simple foundation for testing 
            -> also making sure output is formatted corectly

Function: diff
Test Case Number: 2
Input: (X^^2)*(Sin X) + (Cos X)^^3
Expected Output: 2*X*(Sin X) + (X^^2)*(Cos X) - 3*(X^^2)*(Sin (X^^3))
Actual Output: ((-3.0*((X^^2)*(Sin (X^^3)))) + ((2.0*(X*(Sin X))) + ((X^^2)*(Cos X))))
Rationale: Testing chain rule application and seeing how different derivatives interact

Function: diff
Test Case Number: 3 
Input: ((2*(X^^5) + Sin (1 + 3*X))^^4) * ((4+X)^^3)*(Cos (X^^2) + abs (5*((1+X)^^4)))
Expected Output: ((((((4.0 + X)^^3)*(Cos (X^^2))) + (Abs (5.0*((1.0 + X)^^4))))*((((2.0*(X^^5)) + (Sin (1.0 + (3.0*X))))^^3)*((40.0*(X^^4)) + (12.0*(Cos (1.0 + (3.0*X))))))) + ((((2.0*(X^^5)) + (Sin (1.0 + (3.0*X))))^^4)*((100.0*(((1.0 + X)^^7)*((Abs (5.0*((1.0 + X)^^4)))^^-1))) + ((3.0*(((4.0 + X)^^2)*(Cos (X^^2)))) + (-2.0*((X*(Sin (X^^2)))*((4.0 + X)^^3)))))))
Actual Output: ((((((4.0 + X)^^3)*(Cos (X^^2))) + (Abs (5.0*((1.0 + X)^^4))))*((((2.0*(X^^5)) + (Sin (1.0 + (3.0*X))))^^3)*((40.0*(X^^4)) + (12.0*(Cos (1.0 + (3.0*X))))))) + ((((2.0*(X^^5)) + (Sin (1.0 + (3.0*X))))^^4)*((100.0*(((1.0 + X)^^7)*((Abs (5.0*((1.0 + X)^^4)))^^-1))) + ((3.0*(((4.0 + X)^^2)*(Cos (X^^2)))) + (-2.0*((X*(Sin (X^^2)))*((4.0 + X)^^3)))))))
Rationale: Giving the function my all

------------ pretty ------------

Function: pretty
Test Case Number: 1
Input: Add (Mult (Coef 2) (Power (Add X (Coef 1)) 4)) (Power X 2)
Expected Output: ((2*((X + 1)^^4)) + (X^^2))
Actual Output: ((2*((X + 1)^^4)) + (X^^2))
Rationale: Testing nexted addition multiplication and exponentiation

Function: pretty
Test Case Number: 2
Input: Add (Sin (Mult (Coef 3) X)) (Mult (Coef 4) (Power X 5))
Expected Output: ((Sin (3*X)) + (4*(X^^5)))
Actual Output: ((Sin (3*X)) + (4*(X^^5)))
Rationale: Seeing how function prints trig functions alongside polynomial terms

Function: pretty
Test Case Number: 3
Input: Mult (Abs (Add (Coef (-5)) (Mult (Coef 2) X))) (Cos (Power X 3))
Expected Output: ((Abs (-5 + (2*X)))*(Cos (X^^3)))
Actual Output: ((Abs (-5 + (2*X)))*(Cos (X^^3)))
Rationale: Seeing how function deals with everything combined together
-}
  