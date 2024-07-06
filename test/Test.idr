module Test

import Test.Golden

req : List Requirement
req = [ Node ]

mkTests : List String -> TestPool
mkTests = MkTestPool "Misc"
                  req
                  Nothing

misc : TestPool
misc = mkTests
  [ "help-command"
  ]

configTests : IO TestPool
configTests = testsInDir "config-command" "Config Command"

main : IO ()
main = runner
  [ misc
  , !configTests
  ]

