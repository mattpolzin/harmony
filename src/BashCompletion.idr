module BashCompletion

import Data.Config
import Data.List
import Data.String

import Debug.Trace

%default total

-- given a pair of strings, the first representing the word
-- actually being edited, the second representing the word
-- before the one being edited, return a list of possible
-- completions. If the list of completions is empty, bash
-- will perform directory completion.
export
opts : Config => String -> String -> List String
opts @{config} partialTeamName _ =
  filter (isPrefixOf partialTeamName) config.teamSlugs

export
script : String
script = """
_harmony()
{
  ED=$([ -z $2 ] && echo "--" || echo $2)
  COMPREPLY=($(harmony --bash-completion $ED $3))
}

complete -F _harmony -o default harmony
"""

