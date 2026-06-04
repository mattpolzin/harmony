### Testing
Tests are written in two distinct ways.

#### Golden Tests
Golden Tests can be run with `make test`. You'll need a few common tools (in addition
to the tools required to build harmony) in your `PATH` to run all the tests:
  - `realpath`
  - `sed`
  - `xargs`

#### Inline Tests
Some tests are written into the Harmony codebase. These tests live in `Test.idr` files located in a subfolder for the module being tested. For example, `src/Commands/Graph/Test.idr` contains the `Commands.Graph.Test` module which has tests for the `Commands.Graph` module.

These tests are always going to come in one of two forms of equality check. First, compile-time equality checks: `testSometing : someFun "some string" === "some other string"`. These test that certain inputs to functions result in expected outputs. They require the function to be able to be evaluated at compile time. In cases where a function cannot be fully evaluated at compile time, the second variant can be used: `testSomethingElse : someFun "some string" ==> "some other string"`. In this second variant, the test will be run by `make type-test` rather than at compile time.
