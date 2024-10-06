{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Primitives where
import Defs
import GHC.Float (floorDouble, roundDouble, ceilingDouble)

--- Stack Manip
type Op = Stack Val -> Stack Val

duplicate :: Op
duplicate (S (v:vs)) = S (v:v:vs)
duplicate _ = error "Not enough arguments for dup"

over :: Op
over (S (v1:v2:vs)) = S $ v2:v1:v2:vs
over _ = error "Not enough arguments for over"

around :: Op
around (S (v1:v2:vs)) = S $ v1:v2:v1:vs
around _ = error "Not enough arguments for around"

flip :: Op
flip (S (v1:v2:vs)) = S $ v2:v1:vs
flip _ = error "Not enough arguments for flip"

pop :: Op
pop (S (_:vs)) = S vs
pop _ = error "Not enough arguments for pop"

on :: Op -> Op
on _ (S []) = error "Not enough arguments for onF"
on f s@(S (v : _)) =
  let S vs' = f s
   in S $ v : vs'


--- Constants
eta :: Val
eta = Number $ Prelude.pi / 4

pi :: Val
pi = Number Prelude.pi

tau :: Val
tau = Number $ Prelude.pi * 2


--- Monadic Pervasive
negate :: Op
negate = fmap Prelude.negate

not :: Op
not = fmap (1 -)

abs :: Op
abs = fmap Prelude.abs

sign :: Op
sign = fmap Prelude.signum

sqrt :: Op
sqrt = fmap (propagateMon Prelude.sqrt)

sine :: Op
sine = fmap (propagateMon sin)

round :: Op
round = fmap (propagateMon (fromIntegral . Prelude.round))

ceiling :: Op
ceiling = fmap (propagateMon (fromIntegral .Prelude.ceiling))

floor :: Op
floor = fmap (propagateMon (fromIntegral . Prelude.floor))


-- Dyadic Pervasive
equals :: Op
equals (S (v1:v2:vs)) = undefined
