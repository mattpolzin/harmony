module BashCompletion

import Data.Config
import Data.List
import Data.Maybe
import Data.String

%default total

allRootCmds : List String
allRootCmds = [ "assign"
              , "branch"
              , "config"
              , "contribute"
              , "graph"
              , "help"
              , "label"
              , "list"
              , "pr"
              , "reflect"
              , "sync"
              , "version"
              , "whoami"
              ]

||| Attempt to handle completions for root commands but
||| if we ar not currently on the root command (at least
||| one argument has already been entered), we return
||| @Nothing@ so that code can call out to the full @opts@
||| function after loading the config file.
export
cmdOpts : (subcommand : String) -> (curWord : String) -> (prevWord : String) -> Maybe (List String)
-- first the root commands:
cmdOpts _ "--"       "harmony" = Just allRootCmds
cmdOpts _ partialCmd "harmony" = Just $ filter (isPrefixOf partialCmd) allRootCmds

-- then the subcommands that take no arguments;
cmdOpts "pr"      _ _ = Just []
cmdOpts "sync"    _ _ = Just []
cmdOpts "help"    _ _ = Just []
cmdOpts "--help"  _ _ = Just []
cmdOpts "reflect" _ _ = Just []
cmdOpts "version" _ _ = Just []

-- next subcommands that have options with no configuration requirement:
cmdOpts "contribute" "-"  _ = Just ["--checkout", "-c"]
cmdOpts "contribute" "--" _ = Just ["--checkout", "-c"]
cmdOpts "contribute" partialArg _  =
  if partialArg `isPrefixOf` "--checkout"
     then Just ["--checkout"]
     else Just []
cmdOpts "graph" "--" _ = Nothing
cmdOpts "graph" "-"  _ = Just ["--completed", "-c"]
cmdOpts "graph" partialArg _ =
  if partialArg `isPrefixOf` "--completed"
     then Just ["--completed"]
     else Nothing

-- anything else requires configuration being loaded
cmdOpts _ _ _ = Nothing

export
opts : Config => (subcommand : String) -> (curWord : String) -> (prevWord : String) -> List String
-- we assume we are not handling a root command (see @cmdOpts@ which
-- should have already been called).

-- then the config command
opts @{_} "config" "--" "config" = settablePropNames
opts @{_} "config" partialConfigProp "config" = filter (isPrefixOf partialConfigProp) settablePropNames
opts @{_} "config" _ _ = []

-- and the label command
opts @{config} "label" "--" _ = config.repoLabels
opts @{config} "label" partialLabel _ = filter (isPrefixOf partialLabel) config.repoLabels

-- then list, which only accepts a single team slug:
opts @{config} "list" "--" "list" = config.teamSlugs
opts @{config} "list" partialTeamName "list" =
  filter (isPrefixOf partialTeamName) config.teamSlugs
opts @{config} "list" "--" _ = []

-- then graph, which only accepts a single team slug
opts @{config} "graph" "--" "graph" = "--completed" :: config.teamSlugs
opts @{config} "graph" "--" "--completed" = config.teamSlugs
opts @{config} "graph" partialTeamName previous =
  if isJust $ find (== previous) ["--completed", "graph"]
     then filter (isPrefixOf partialTeamName) config.teamSlugs
     else []

-- finally, assign auto-completes with 
-- either a team slug or '+' followed by a user login:
opts @{config} "assign" "--" "assign" = "--dry" :: config.teamSlugs
opts @{config} "assign" "--" _ = config.teamSlugs
opts @{config} "assign" partialArg _ =
  if partialArg `isPrefixOf` "--dry"
     then ["--dry"]
     else filter (isPrefixOf partialArg) slugsOrLogins
  where
    -- If the word being typed is prefixed with '+' return user logins
    -- but otherwise return team slugs. 
    slugsOrLogins : List String
    slugsOrLogins =
      if "+" `isPrefixOf` partialArg
        then (strCons '+') <$> config.orgMembers
        else config.teamSlugs

opts @{_} _ _ _ = []

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

