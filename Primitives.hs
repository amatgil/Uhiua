module Primitives where
import Defs

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
pop (S (v1:vs)) = S vs
pop _ = error "Not enough arguments for pop"

on :: Op -> Op
on f (S []) = error "Not enough arguments for onF"
on f s@(S (v : vs)) =
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
