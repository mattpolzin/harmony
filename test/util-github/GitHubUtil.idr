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
  putStrLn "-- without issue number --"
  testBranchParsing "Jira-1234/hello"
