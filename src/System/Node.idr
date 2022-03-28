module System.Node

%foreign "node:lambda:(cmd) => require('child_process').spawnSync(cmd, [], {shell: true, stdio: 'inherit'}).status"
prim__system : String -> PrimIO Int

||| Execute a shell command, returning its termination status or -1 if an error
||| occurred.
export
system : HasIO io => String -> io Int
system cmd = primIO (prim__system cmd)
