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
  (+) (Char c1) (Char c2) = error "Cannot add characters"
  (+) (Array s1 vs1) (Array s2 vs2) = error "Cannot add arrays yet '>.>"
  (-) :: Val -> Val -> Val
  (-) v1 v2 = (+) v1 $ negate v2
  (*) :: Val -> Val -> Val
  (*) = undefined
  negate :: Val -> Val
  negate (Number x) = Number $ -x
  negate (Comp c) = Comp $ -c
  negate (Char c) = error "Cannot negate char"
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
  fmap f (S (v:vs)) = S (f v : fmap f vs)

instance Applicative Stack where
  pure :: a -> Stack a
  pure = S . singleton
  (<*>) :: Stack (a -> b) -> Stack a -> Stack b
  (<*>) (S (f : fs)) (S (x : xs)) = S (f x : (fs <*> xs))
  (<*>) _ _ = S []

-- Not a Monad, I don't think! 
