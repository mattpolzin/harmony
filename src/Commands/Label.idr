module Commands.Label

import Data.Config
import Data.List
import Data.Promise
import Data.PullRequest
import Data.String

import Util.ShellCompletion

import FFI.GitHub

%default total

||| In order to support tab completion of multi-word labels, spaces have been turned into
||| another character to "slugify" the labels. Still, it is possible the user has entered
||| a label that literally contains the character used during slugification, so to
||| unslugify, we first see if a label appears in the configured list of labels. If it does
||| then we use it exactly but if it doesn't then we unslugify it before using it.
export
unslugifyLabel : (configLabels : List String) -> (slugifiedLabel : String) -> String
unslugifyLabel configLabels slugifiedLabel =
  case find (== slugifiedLabel) configLabels of
       Just label => label
       Nothing    => unslugify $ unhashify slugifiedLabel

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

