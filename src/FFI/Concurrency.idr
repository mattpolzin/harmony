module FFI.Concurrency

import Data.Either
import Data.List1
import Data.Promise
import Data.Vect
import FFI
import JSON.Parser

%default total

concurrency_ffi : String -> String
concurrency_ffi = node_ffi "concurrency"

export
data Future : Type where [external]

%foreign concurrency_ffi "future"
prim__future : (args : String) -> PrimIO Future

export
fork : HasIO io => (args : String) -> io Future
fork args = primIO $ prim__future args

%foreign "node:lambda:(p1,p2)=>Promise.all([p1,p2])"
prim__both : Future -> Future -> PrimIO Future

%foreign "node:lambda:(p)=>Promise.all([p])"
prim__singleton : Future -> PrimIO Future

%foreign "node:lambda:()=>Promise.all([])"
prim__neutral : PrimIO Future

%foreign "node:lambda:(p)=>{return new Promise((onSuccess, onFailure) => p.then(r => onSuccess(r.flat()), onFailure))}"
prim__flatten : Future -> PrimIO Future

all : HasIO io => Foldable t => t Future -> io Future
all xs = foldr both (primIO prim__neutral) xs
  where
    both : Future -> io Future -> io Future
    both x y = do
      x'     <- primIO $ prim__singleton x
      nested <- primIO $ prim__both x' !y
      primIO $ prim__flatten nested

%foreign concurrency_ffi "await_stringify"
prim__awaitStringify : Future -> (onSuccess : String -> PrimIO ()) -> (onError : String -> PrimIO ()) -> PrimIO ()

||| Create a Promise that waits for all the given futures to finish
||| and then processes the result (an array of results) as a list
||| of JSON objects.
export
promiseAll : Foldable t => t Future -> Promise (List JSON)
promiseAll xs =
  do f <- all xs
     str <- promisify $ \ok,err => prim__awaitStringify f (\x => toPrim $ ok x) (\y => toPrim $ err y)
     JArray xs <- either . mapFst (const "Failed to parse JSON from \{str}") $ parseJSON Virtual str
       | other => reject "Expected a JSON array from futures but got \{show other}."
     pure xs

