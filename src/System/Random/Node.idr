module System.Random.Node

import Data.List

%default total

-- TODO: Remove this once Idris2 gains a version newer than 0.5.1. This has been added to the base library.

%foreign "node:lambda:() => Math.random()"
prim__rnd : PrimIO Double

||| Generate a random number between 0 and 1.
export
rnd : HasIO io => io Double
rnd = primIO $ prim__rnd

export
rndSelect : HasIO io => (elems : List a) -> (0 _ : NonEmpty elems) => io a
rndSelect elems = 
  do randomDouble <- rnd 
     let randomNat : Nat = cast . floor $ randomDouble * cast (length elems)
     pure $ 
       case inBounds randomNat elems of
            (No _)  => head elems
            (Yes _) => index randomNat elems

