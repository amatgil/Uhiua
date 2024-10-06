module Defs where
import Data.List
import Data.Complex
---- DEFS ----
data Val
  = Number Double
  | Comp (Complex Double)
  | Char Char
  | Array [Word] [Val]

newtype Stack v = S [v]

instance Num Val where
  (+) :: Val -> Val -> Val 
  (+) (Number n1) (Number n2) = Number $ n1 + n2
  (+) (Comp c1) (Comp c2) = Comp $ c1 + c2
  (+) (Char _) (Char _) = error "Cannot add characters"
  (+) (Array s1 vs1) (Array s2 vs2) = if s1 == s2 then Array s1 (zipWith (+) vs1 vs2) else error "Array shapes do not match"
  (+) x y = error $ "Type error: cannot add " ++ typeOfVal x ++ " and " ++ typeOfVal y

  (-) :: Val -> Val -> Val
  (-) v1 v2 = (+) v1 $ negate v2

  (*) :: Val -> Val -> Val
  (*) (Number n1) (Number n2) = Number $ n1 * n2
  (*) (Comp c1) (Comp c2) = Comp $ c1 * c2
  (*) (Char _) (Char _) = error "Cannot multiply characters"
  (*) (Array s1 vs1) (Array s2 vs2) = if s1 == s2 then Array s1 (zipWith (*) vs1 vs2) else error "Array shapes do not match"
  (*) x y = error $ "Type error: cannot multiply " ++ typeOfVal x ++ " and " ++ typeOfVal y

  negate :: Val -> Val
  negate (Number x) = Number $ -x
  negate (Comp c) = Comp $ -c
  negate (Char _) = error "Cannot negate char"
  negate (Array shape vals) = Array shape (fmap negate vals)

  abs :: Val -> Val
  abs = undefined 

  signum :: Val -> Val
  signum = undefined 

  fromInteger :: Integer -> Val
  fromInteger = Number . fromInteger

instance Show Val where
  show :: Val -> String
  show (Number x) = show x
  show (Comp c) = show c
  show (Char c) = "@" ++ show c
  show (Array shape values) = "Shape: " ++ show shape ++ " ==> "++ show values

instance Show v => Show (Stack v) where
  show :: Stack v -> String
  show (S vs) = intercalate "\n" (go vs) where
    go :: [v] -> [String]
    go [] = [""]
    go (x:xs) = show x : go xs

instance Functor Stack where
  fmap :: (a -> b) -> Stack a -> Stack b
  fmap _ (S []) = S []
  fmap f (S (v:vs)) = S (f v : fmap f vs)

instance Applicative Stack where
  pure :: a -> Stack a
  pure = S . singleton
  (<*>) :: Stack (a -> b) -> Stack a -> Stack b
  (<*>) (S (f : fs)) (S (x : xs)) = S (f x : (fs <*> xs))
  (<*>) _ _ = S []

-- Not a Monad, I don't think! 


propagateMon :: (Double -> Double) -> Val -> Val
propagateMon f v = case v of
  (Number x) -> Number $ f x
  (Comp (r :+ i)) -> Comp $ f r :+ f i
  (Char _) -> error "Cannot apply monad to Char"
  (Array shape vs) -> Array shape (fmap (propagateMon f) vs)

typeOfVal :: Val -> String
typeOfVal (Number _) = "Number"
typeOfVal (Comp _) = "Complex"
typeOfVal (Char _) = "Char"
typeOfVal  (Array _ _) = "Array"
