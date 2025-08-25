import Util.Github

testBranchParsing : String -> IO ()
testBranchParsing branch =
  printLn $ parseGithubIssueNumber branch

main : IO ()
main = do
  putStrLn "-- with issue number --"
  -- shrink from front
  testBranchParsing "feature/1234/hello"
  testBranchParsing "/1234/hello"
  testBranchParsing "1234/hello"
  -- shrink from back
  testBranchParsing "feature/1234/"
  testBranchParsing "feature/1234"
  -- just number
  testBranchParsing "/1234/"
  testBranchParsing "1234/"
  testBranchParsing "/1234"
  testBranchParsing "1234"
  putStrLn ""
  putStrLn "-- with issue number and GH- prefix --"
  -- shrink from front
  testBranchParsing "feature/GH-1234/hello"
  testBranchParsing "/GH-1234/hello"
  testBranchParsing "GH-1234/hello"
  -- shrink from back
  testBranchParsing "feature/GH-1234/"
  testBranchParsing "feature/GH-1234"
  -- just number
  testBranchParsing "/GH-1234/"
  testBranchParsing "GH-1234/"
  testBranchParsing "/GH-1234"
  testBranchParsing "GH-1234"
  putStrLn ""
  putStrLn "-- without issue number --"
  testBranchParsing "Jira-1234/hello"
  testBranchParsing "feature1234/hello"
  testBranchParsing "feature/GHI1234/hello"
  testBranchParsing "featureGH-1234/hello"
