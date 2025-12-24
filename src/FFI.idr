module FFI

import Data.Promise

%default total

promiseIO : (primFn : (String -> PrimIO ()) -> (String -> PrimIO ()) -> PrimIO ()) -> Promise' String
promiseIO primFn =
  promisify $ \ok,notOk => primFn (\res => toPrim $ ok res) (\err => toPrim $ notOk err)

node_ffi : (libName : String) -> (fnName : String) -> String
node_ffi libName fnName = "node:support:\{fnName},\{libName}"

