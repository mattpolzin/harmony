module Commands.Label

import Data.Config
import Data.List
import Data.Promise
import Data.PullRequest
import Data.String

import BashCompletion

import FFI.GitHub

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
       Nothing    => BashCompletion.unslugify $ BashCompletion.unhashify slugifiedLabel

namespace TestUnslugifyLabel
  test1 : unslugifyLabel ["hello", "world"] "hello" = "hello"
  test1 = Refl

  test2 : unslugifyLabel ["hello", "world"] "#world" = "world"
  test2 = Refl

  test3 : unslugifyLabel ["hello", "world"] "\\#hello" = "hello"
  test3 = Refl

  test4 : unslugifyLabel ["hello world"] "hello world" = "hello world"
  test4 = Refl

  test5 : unslugifyLabel ["hello world"] "#hello world" = "hello world"
  test5 = Refl

  test6 : unslugifyLabel ["hello world"] "\\#hello world" = "hello world"
  test6 = Refl

  test7 : unslugifyLabel ["hello world"] "hello◌world" = "hello world"
  test7 = Refl

  test8 : unslugifyLabel ["hello world"] "#hello◌world" = "hello world"
  test8 = Refl

  test9 : unslugifyLabel ["hello world"] "\\#hello◌world" = "hello world"
  test9 = Refl


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

