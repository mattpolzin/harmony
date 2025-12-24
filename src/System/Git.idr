module System.Git

import Data.Promise
import Data.String

import Util.System

import Language.Reflection
import Derive.Prelude

%language ElabReflection

%default total

public export
data BranchStatus = Existing | New

%runElab derive `{BranchStatus} [Eq]

git : HasIO io => List String -> io (String, String, Int)
git args = assert_total $ run' ("git" :: args)

stdout : (String, String, Int) -> String
stdout = fst

mapStdout : (String -> a) -> (String, String, Int) -> (a, String, Int)
mapStdout = mapFst

||| Drop the given char from the String throughout and for any number of
||| occurrences.
dropChar : Char -> String -> String
dropChar c = pack . go . unpack
  where
    go : List Char -> List Char
    go [] = []
    go (x :: xs) =
      let rest = go xs
      in if c == x then rest else (x :: rest)

promise : IO (a, String, Int) -> Promise' a
promise gitOp = liftIO res >>= either
  where
    res : IO (Either String a)
    res =  do
      (stdout, stderr, exitCode) <- gitOp
      case exitCode of
           0 => pure $ Right stdout
           _ => pure $ Left stderr

export
currentBranch : Promise' String
currentBranch = map trim . promise $ git ["branch", "--show-current"]

parseBranchList : String -> List String
parseBranchList = map trim . lines . dropChar '*'

export
listBranches : Promise' (List String)
listBranches = 
  promise $
    mapStdout parseBranchList <$> (git ["branch", "--list"])

export
listBranches' : HasIO io => io (List String)
listBranches' = 
  parseBranchList . stdout <$> (git ["branch", "--list"])

export
checkoutBranch : {default Existing b : BranchStatus} -> (branch : String) -> Promise' ()
checkoutBranch {b=branchStatus} branch = 
  let constantArgs = [branch]
      args = case branchStatus of
                  New => ("-b" :: constantArgs)
                  Existing => constantArgs
  in ignore . promise $ git ("checkout" :: args)

