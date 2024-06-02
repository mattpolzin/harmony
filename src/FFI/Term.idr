module FFI.Term

%default total

%foreign "node:lambda:()=>(process.stdout.columns ? BigInt(process.stdout.columns) : -1)"
prim__termCols : PrimIO Int

export
termCols : IO (Maybe Nat)
termCols =
  [ (if cols == -1 then Nothing else Just $ cast cols) | cols <- primIO prim__termCols]

