module ShellCompletion.Bash

import public ShellCompletion.Common

import Data.List
import Data.String
import Data.Promise

%default total

||| The Bash Completion script calls to harmony with a special --bash-completion
||| flag and passes harmony the subcommand (i.e. first argument after harmony),
||| the current argument being edited, and the previous argument.
|||
||| If any of those are empty strings or just not available yet, double dash is used.
|||
||| For example:
|||   Tab completion if the user has entered `harmony ` would call
|||   `harmony --bash-completion "--" "--" "harmony"`
|||
|||   Tab completion if the user has entered `harmony gr` would call
|||   `harmony --bash-completion "gr" "gr" "harmony"`
|||
|||   Tab completion if the user has entered `harmony graph de` would call
|||   `harmony --bash-completion "graph" "de" "graph"`
|||
|||   Tab completion if the user has entered `harmony graph developers fl` would call
|||   `harmony --bash-completion "graph" "fl" "developers"
export
script : String
script = """
_harmony()
{
  CURRENT_PARTIAL=$([ -z $2 ] && echo "--" || echo "$2")
  PREVIOUS="$3"
  SUBCOMMAND=$([ -z ${COMP_WORDS[1]} ] && echo "--" || echo "${COMP_WORDS[1]}")
  COMPREPLY=($(harmony --bash-completion "$SUBCOMMAND" "$CURRENT_PARTIAL" "$PREVIOUS"))
}

complete -F _harmony harmony
"""

