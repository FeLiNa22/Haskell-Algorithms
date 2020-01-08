module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp 
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  -- Extension
  fromInteger    = undefined
  negate (Val 0) = (Val 0)
  negate x       = UnApp Neg x
  (+) (Val 0) y  = y
  (+) x (Val 0)  = x
  (+) x y        = BinApp Add x y
  (*) (Val 1) y  = y
  (*) x (Val 1)  = x
  (*) x y        = BinApp Mul x y
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  -- Extension
  fromRational  = undefined  
  (/) (Val 0) y = (Val 0)
  (/) x (Val 1) = x
  (/) x y       = BinApp Div x y
-- Leave the following one undefined...
  recip         = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre : The element exists in the list
lookUp e lst = fromJust $ lookup e lst

showExp :: Exp -> String
showExp (UnApp unOp e) = function_lookup ++ "(" ++ next_exp ++ ")" 
  where 
    function_lookup = lookUp unOp functions
    functions       = [(Neg,("-")),(Sin,("sin")),(Cos,("cos")),(Log,("log"))]
    next_exp        = showExp e 

showExp (BinApp binOp e e')  = "(" ++ (left_exp) ++ (function_lookup) ++ (right_exp) ++ ")"
  where 
    function_lookup = lookUp binOp functions
    functions       = [(Mul,"*"),(Div,"/"),(Add,"+")]
    left_exp        = showExp e 
    right_exp       = showExp e' 

showExp (Val x)  = show x
showExp (Id key) = key

eval :: Exp -> Env -> Double
-- data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
eval (UnApp unOp e) env = function_lookup next_exp 
  where 
    function_lookup = lookUp unOp functions
    functions       = [(Neg,(negate)),(Sin,(sin)),(Cos,(cos)),(Log,(log))]
    next_exp        = eval e env

eval (BinApp binOp e e') env = function_lookup left_exp right_exp
  where 
    function_lookup = lookUp binOp functions
    functions       = [(Mul,(*)),(Div,(/)),(Add,(+))]
    left_exp        = eval e env
    right_exp       = eval e' env

eval (Val x) _      = x
eval (Id key) env   = lookUp key env 


diff :: Exp -> String -> Exp
diff (BinApp binOp e e') var = differential
  where 
    u              = diff e var
    v              = diff e' var
    a              = BinApp Mul e v 
    b              = BinApp Mul u e'
    differential   = lookUp binOp function_diffs
    function_diffs = [(Mul, (BinApp Add a b) ), (Div, (BinApp Div (BinApp Add a (UnApp Neg b)) (BinApp Mul e' e')) ), (Add, (BinApp Add u v))]

diff (UnApp unOp e) var = differential
  where 
    u              = diff e var
    differential   = lookUp unOp function_diffs
    function_diffs = [(Neg,(UnApp Neg u)), (Sin,(BinApp Mul (UnApp Cos e) u)), (Cos,(UnApp Neg (BinApp Mul (UnApp Sin e) u))), (Log,(BinApp Div u e))]

diff (Val _) _ = Val 0 
diff (Id x) var 
  | x == var  = Val 1
  | otherwise = Val 0



--Extension using overloading
diff2 :: Exp -> String -> Exp
diff2 (BinApp binOp e e') var = differential
  where 
    u              = diff e var
    v              = diff e' var
    a              = e * v 
    b              = u * e'
    differential   = lookUp binOp function_diffs
    function_diffs = [(Mul, (a + b)), (Div, ((a + (-b)) / (e' * e'))), (Add, (u + v))]

diff2 (UnApp unOp e) var = differential
  where 
    u              = diff e var
    differential   = lookUp unOp function_diffs
    function_diffs = [(Neg, (-u)), (Sin,((cos e) * u)), (Cos,(-((sin e) * u))), (Log,(u / e))]

diff2 (Val _) _  = Val 0 

diff2 (Id x) var 
  | x == var     = Val 1
  | otherwise    = Val 0

  
-- maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp point nterms = sum $ maclaurin_terms
  where    
    differentials    = take nterms (iterate (flip diff "x")  exp)
    fn0              = map (flip eval [("x",0)]) differentials
    factorials       = scanl (*) 1 [(1.0)..]
    maclaurin_terms = zipWith3 (\x y z -> ((x / y) * (point ^ z))) fn0 factorials [0..]

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
