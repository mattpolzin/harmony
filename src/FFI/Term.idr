module FFI.Term

%default total

%foreign "node:lambda:()=>BigInt(process.stdout.columns)"
prim__termCols : PrimIO Int

export
termCols : IO Nat
termCols = cast <$> primIO prim__termCols

