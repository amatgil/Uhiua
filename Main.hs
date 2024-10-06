module Main where
import Lib
import Prelude hiding (flip, Char)
import Data.Complex

main =
  let s =
        S
          [ Number 1,
            Number 2,
            Number 3,
            Comp $ 10 :+ 11,
            Char 'a',
            Array [4] [Number 1, Number 1, Number 1, Number 1],
            Array [2, 2] [Number 1, Number 1, Number 1, Number 1]
          ]
   in print $ on flip s
