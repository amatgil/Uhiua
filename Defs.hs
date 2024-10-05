module Defs where
import Data.List
---- DEFS ----
data Val
  = Number Double
  | Complex Double Double
  | Char Char
  | Array [Word] [Val]

newtype Stack v = S [v]


instance Show Val where
  show :: Val -> String
  show (Number x) = show x
  show (Complex r i) = show r ++ "r" ++ show i ++ "i"
  show (Char c) = "@" ++ show c
  show (Array shape values) = show values

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
