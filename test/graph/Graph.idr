import Commands.Graph
import Data.Config
import Util
import Data.SortedMap
import Data.Theme

config : Config
config =
    MkConfig {
        updatedAt         = 0
      , org               = "org"
      , repo              = "repo"
      , defaultRemote     = "origin"
      , mainBranch        = "main"
      , requestTeams      = True
      , requestUsers      = True
      , teamSlugs         = ["team1", "team2"]
      , repoLabels        = ["label1", "label2", "label3"]
      , commentOnRequest  = None
      , branchParsing     = None
      , orgMembers        = ["sam", "gretta", "florence"]
      , ignoredPRs        = []
      , githubPAT         = Nothing
      , theme             = Dark
      , ephemeral         = MkEphem "path/to/repo" False 200 Nothing
      }

testReviewsGraph : ({x : Type} -> x -> Maybe x) -> IO ()
testReviewsGraph f =
  let cfg = config
      graph = Commands.Graph.reviewsGraph {login=String} 
                ["sam", "gretta", "gretta", "gretta", "florence", "florence"]
                ["sam", "sam", "gretta"]
                ["sam", "gretta", "florence"]
                (f $ fromList [("sam", 3), ("florence", 2)])
  in  Util.renderIO graph

main : IO ()
main = do
  putStrLn "-- with completed --"
  testReviewsGraph Just
  putStrLn ""
  putStrLn "-- without completed --"
  testReviewsGraph (const Nothing)
