module Commands.Label

import Data.Config
import Data.Promise
import Data.PullRequest

import FFI.GitHub

||| Add labels to a Pull Request.
|||
||| Returns a list of all previously and newly applied
||| labels.
export
addLabels : Config => Octokit =>
            PullRequest
        -> (labels : List String)
        -> Promise' (List String)
addLabels @{config} pr labels =
  addPullLabels config.org config.repo pr.number labels

