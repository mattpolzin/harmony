module Test

import Data.SnocList
import Data.String
import Test.Golden
import System.File
import System

%default total

splitOn : (sbustr : String) -> String -> Maybe (String, String)
splitOn substr str = 
  let sub = unpack substr
  in mapHom pack <$> go sub sub [<] [<] (unpack str)
  where
    go : (fullSubstr : List Char) -> (remainingToMatch : List Char) -> (beforeMatch : SnocList Char) -> (throughMatch : SnocList Char) -> List Char -> Maybe (List Char, List Char)
    go fullSubstr [] beforeMatch throughMatch cs = Just (cast beforeMatch, cs)
    go fullSubstr (x :: xs) beforeMatch throughMatch [] = Nothing
    go fullSubstr (x :: xs) beforeMatch throughMatch (y :: ys) =
      if x == y
         then go fullSubstr xs beforeMatch (throughMatch :< y) ys
         else go fullSubstr fullSubstr (throughMatch :< y) (throughMatch :< y) ys

req : List Requirement
req = []

mkTests : String -> List String -> TestPool
mkTests name = MkTestPool name
                          req
                          Nothing

unit : TestPool
unit = mkTests "Unit"
  [ "graph"
  ]

misc : TestPool
misc = mkTests "Misc"
  [ "help-command"
  ]

configTests : IO TestPool
configTests = testsInDir "config-command" "Config Command"

||| We want to avoid Harmony performing a sync as part of tests that read the
||| test configuration file so we mark the configuration file as updated.
covering
markConfigsAsUpdated : IO ()
markConfigsAsUpdated = do
  Right config <- readFile "./config-command/harmony.json"
    | Left e => die (show e)
  let Just (before, after) = splitOn #""updatedAt":"# config
    | Nothing => die "could not update config timestamp"
  let (_, rest) = break (\c => c == ',' || c == '}') after
  let ts = show !time
  let config' = before ++ "\"updatedAt\":" ++ ts ++ rest
  Right _ <- writeFile "./config-command/harmony.json" config'
    | Left e => die (show e)
  pure ()

covering
main : IO ()
main = do
  markConfigsAsUpdated
  runner
    [ unit
    , misc
    , !configTests
    ]

