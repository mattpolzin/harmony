module FFI.Concurrency

import FFI
import Data.List1
import Data.Vect
import Data.Either
import Data.Promise
import Language.JSON

%default total

export
data Future : Type where [external]

%foreign "node:lambda:(args)=>{return new Promise((onSuccess, onFailure) => { require('child_process').exec(`harmony ${args}`,(error, stdout, stderr) => { if (error) { onFailure(stderr); } else { onSuccess(stdout); }})})}"
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

all : HasIO io => List Future -> io Future
all []  = primIO $ prim__neutral
all [x] = primIO $ prim__singleton x
all (x :: [y]) = primIO $ prim__both x y
all (x :: (y :: (z :: xs))) =
  do rest <- all (z :: xs)
     head <- (primIO $ prim__both x y)
     nested <- primIO $ prim__both head rest
     primIO $ prim__flatten nested

%foreign "node:lambda:(p,s,e)=>p.then(r => { s(JSON.stringify(Array.isArray(r) ? r.map(JSON.parse) : r))(); },err => { e(err)(); })"
prim__awaitStringify : Future -> (onSuccess : String -> PrimIO ()) -> (onError : String -> PrimIO ()) -> PrimIO ()

||| Create a Promise that waits for all the given futures to finish
||| and then processes the result (an array of results) as a list
||| of JSON objects.
export
promise : List Future -> Promise (List JSON)
promise xs =
  do f <- all xs
     str <- promisify $ \ok,err => prim__awaitStringify f (\x => toPrim $ ok x) (\y => toPrim $ err y)
     JArray xs <- either . maybeToEither "Failed to parse JSON from \{str}" $ parse str
       | other => reject "Expected a JSON array from futures but got \{show other}."
     pure xs

