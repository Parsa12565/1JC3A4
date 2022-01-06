{- Assignment 4
 - Name: Parsa Zanganeh
 - Date: 2020-11-26
 -}
module Assign_4 where

macid :: String
macid = "zanganep"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                abs (2*X + 3 + 4)  * X
 -          can be encoded as
 -                Prod [ Abs (Sum [Prod [Coef 2.0,X],Coef 3,Coef 4]),X]
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Sum (MathExpr a) (MathExpr a)
  | Prod (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
  deriving (Eq,Show,Read)

{- --------------------------------------------------------------------
 - Function: eval
 - --------------------------------------------------------------------
 - Description: Finds the value of a given mathematical expression at X = the given value
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v = v
eval (Coef a) _ = a
eval (Sum a b) v = eval a v + eval b v
eval (Prod a b) v = eval a v * eval b v
eval (Power a b) v = eval a v ^^ b
eval (Cos a) v = cos (eval a v)
eval (Sin a) v = sin (eval a v)
eval (Abs a) v = abs (eval a v)

{- --------------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - --------------------------------------------------------------------
 - Description: Implements methods from Num for MathExpr
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Sum x y
  x * y         = Prod x y
  negate x      = Prod (Coef (-1)) x
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      =  error "un-implemented"

{- --------------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - --------------------------------------------------------------------
 - Description: Implements methods from Fractional for MathExpr
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e (-1)
  fromRational e = Coef (fromRational e)

{- --------------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - --------------------------------------------------------------------
 - Description: Implements methods from Floating for MathExpr
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin     = Sin
  cos     = Cos
  log     = error "un-implemented"
  asin _  = error "un-implemented"
  acos _  = error "un-implemented"
  atan _  = error "un-implemented"
  sinh _  = error "un-implemented"
  cosh _  = error "un-implemented"
  tanh _  = error "un-implemented"
  asinh _ = error "un-implemented"
  acosh _ = error "un-implemented"
  atanh _ = error "un-implemented"
  exp _   = error "un-implemented"
  sqrt _  = error "un-implemented"

{- ------------------------------------------------------------------
 - diff
 - ------------------------------------------------------------------
 - Description: Symbolically differentiates a given mathematical expression using the differential rules
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = 1
diff (Coef _) = 0
diff (Sum a b) = diff a + diff b
diff (Prod a b) = Sum (Prod (diff a) b) (Prod a (diff b))
diff (Power a b) = Prod (Prod (Coef (fromInteger (toInteger b))) (Power a (b-1))) (diff a)
diff (Cos a) = Prod (Prod (Coef (-1)) (Sin a)) (diff a)
diff (Sin a) = Prod (Cos a) (diff a)
diff (Abs a) = Prod (Prod a (Power (Abs a) (-1))) (diff a)

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description: Creates a String representation of a given mathematical expression
 -}
pretty :: (Show a) => MathExpr a -> String
pretty X = "X"
pretty (Coef a) = show a
pretty (Sum a b) = "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
pretty (Prod a b) = "(" ++ pretty a ++ " * " ++ pretty b ++ ")"
pretty (Power a b) = "(" ++ pretty a ++ " ^^ " ++ show b ++ ")"
pretty (Cos a) = "cos(" ++ pretty a ++ ")"
pretty (Sin a) = "sin(" ++ pretty a ++ ")"
pretty (Abs a) = "abs(" ++ pretty a ++ ")"

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
Fuction: eval
Property: eval X 1 == 1
Actual Test Result: Pass

Function: eval
Test Case Number: 1
Input: 1 1
Excpected Output: 1.0
Actual Output: 1.0

Function: eval
Test Case Number: 2
Input: X 2
Excpected Output: 2.0
Actual Output: 2.0

Function: eval
Test Case Number: 3
Input: ((X-3)^^2) 3
Excpected Output: 0.0
Actual Output: 0.0

Fuction: diff
Property: diff X == 1
Actual Test Result: Pass

Function: diff
Test Case Number: 1
Input: X
Excpected Output: Coef 1.0
Actual Output: Coef 1.0

Function: diff
Test Case Number: 2
Input: Power X 2
Excpected Output: Prod (Prod (Coef 2.0) (Power X 1)) (Coef 1.0)
Actual Output: Prod (Prod (Coef 2.0) (Power X 1)) (Coef 1.0)

Function: diff
Test Case Number: 3
Input: cos X
Excpected Output: Prod (Prod (Coef (-1.0)) (Sin X)) (Coef 1.0)
Actual Output: Prod (Prod (Coef (-1.0)) (Sin X)) (Coef 1.0)

Fuction: pretty
Property: pretty (Coef 1) == show 1
Actual Test Result: Pass

Function: pretty
Test Case Number: 1
Input: Sum 2 X
Excpected Output: "(2 + X)"
Actual Output: "(2 + X)"

Function: pretty
Test Case Number: 2
Input: Prod X (Power X 2)
Excpected Output: "(X * (X ^^ 2))"
Actual Output: "(X * (X ^^ 2))"

Function: pretty
Test Case Number: 3
Input: abs (Sum (Power X 2) X)
Excpected Output: "abs(((X ^^ 2) + X))"
Actual Output: "abs(((X ^^ 2) + X))"
-}
