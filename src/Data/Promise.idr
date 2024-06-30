module Data.Promise

%default total

-- Promise abstraction from NodeJS

-- Heavily inspired by
-- https://raw.githubusercontent.com/andorp/order-taking/main/src/Service/NodeJS/Promise.idr

||| Promise monad
|||
||| Encapsulates the JavaScript abstraction of promise, which consists of a continuation
||| for successful computation and a continuation for errorneous computation. The promise
||| abstraction bounds these two continuations.
export
record Promise e a where
  constructor MkPromise
  cmd : (a -> IO ()) -> (e -> IO ()) -> IO ()

%name Promise p, p1, p2

||| Activate the reject branch of the Promise with the given error.
export
reject : e -> Promise e a
reject error = MkPromise (\ok, err => err error)

-- Promise is a functor, as we can pre-apply the 'f : a -> b' to the 'a' in the first computation.
-- Hint: This is possible as because the 'a' is in a positive position, because it got twice negated.
export
Functor (Promise e) where
  map f (MkPromise cmd) = MkPromise (\succ => \err => cmd (\x => succ (f x)) err)

bind : Promise e a -> (a -> Promise e b) -> Promise e b
bind (MkPromise cmd) f = MkPromise (\succ =>
                                      \err =>
                                              cmd (\x =>
                                                        let (MkPromise cmd_) = (f x)
                                                        in cmd_ succ err
                                                  ) err
                                    )

-- The Applicative instance of the Promise uses the succ
-- continuation with the value injected by the 'pure' function.
-- The '<*>' operator is based on the Monad's bind implementation.
export
Applicative (Promise e) where
  pure x = MkPromise (\succ => \err => succ x)
  x <*> y = x `bind` (\f => f <$> y)

-- Promise is like the continuation monad, we can create a new command
-- function applying the result of the success computation in the
-- 'f' continuation which computes another Promise that can be unwrapped.
export
Monad (Promise e) where
  (>>=) = bind

export
Alternative (Promise String) where
  empty = reject "empty"

  p1 <|> p2 =
    MkPromise (\ok, err => p1.cmd ok (\_ => p2.cmd ok err))

||| The Promise monad under the hood relies on the IO monad, for that
||| reason we can use the IO monad, execute the IO computation and
||| evaluate the success computation with the result.
export
HasIO (Promise e) where
  liftIO x = MkPromise (\ok => \err => x >>= ok)

||| Resolve a promise compuation.
|||
||| When the final computations for successful branch and errorneous
||| branch is given, the promise computation can be resolved, and if the
||| successful branch finished then its result is passed to the success
||| 'a -> IO ()' computation. If the errorneus branch finishes, the
||| given 'String -> IO ()' evalautes with the result error String.
export
resolve : Promise e a -> (a -> IO ()) -> (e -> IO ()) -> IO ()
resolve (MkPromise cmd) = cmd

||| Alternative version of the resolve function with different argument order.
export
resolve' : (a -> IO ()) -> (e -> IO ()) -> Promise e a -> IO ()
resolve' ok err (MkPromise cmd) = cmd ok err

||| Direct translation between an Either's left and a promises error
||| or an Either's right and a promises success.
export
either : Either e a -> Promise e a
either (Left x)  = reject x
either (Right x) = pure x

||| On Left convert it to String and reject the promise, on Right resolve it.
export
either' : Show e => Either e a -> Promise String a
either' (Left x)  = reject $ show x
either' (Right x) = pure x

||| Helper function definition which encapsulates the essence of
||| the Promise type, PrimIO is needed for the FFI parts to
||| be easily wired in.
public export
PromiseShape : Type -> Type -> Type
PromiseShape e a = (a -> IO ()) -> (e -> IO ()) -> PrimIO ()

||| Turn an FFI promise like construction into a Promise.
|||
||| The name of this function comes from its JavaScript
||| counterpart, which is also called promisify and accepts
||| two closures as arguments.
export
promisify : PromiseShape e a -> Promise e a
promisify prim =
  MkPromise (\ok, err => primIO $ prim ok err)

||| A Promise type with a string error.
public export
%inline
Promise' : Type -> Type
Promise' = Promise String
