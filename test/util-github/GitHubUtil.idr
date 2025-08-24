import Util.Github

testBranchParsing : String -> IO ()
testBranchParsing branch =
  printLn $ parseGithubIssueNumber branch

main : IO ()
main = do
  putStrLn "-- with issue number --"
  testBranchParsing "feature/1234/hello"
  putStrLn ""
  putStrLn "-- without issue number --"
  testBranchParsing "Jira-1234/hello"
