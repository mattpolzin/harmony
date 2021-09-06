module BashCompletion

import Data.Config
import Data.List
import Data.String

import Debug.Trace

%default total

allRootCmds : List String
allRootCmds = [
    "assign"
  , "help"
  , "list"
  , "pr"
  , "sync"
  ]

-- given a pair of strings, the first representing the word
-- actually being edited, the second representing the word
-- before the one being edited, return a list of possible
-- completions. If the list of completions is empty, bash
-- will perform directory completion.
export
opts : Config => String -> String -> List String
-- first the subcommands available:
opts @{_} "--" "harmony" = allRootCmds
opts @{_} partialCmd "harmony" = filter (isPrefixOf partialCmd) allRootCmds

-- then the subcommands that take no arguments
opts @{_} _ "pr"     = []
opts @{_} _ "sync"   = []
opts @{_} _ "help"   = []
opts @{_} _ "--help" = []

-- then list, which only accepts a single team slug:
opts @{config} "--" "list"   = config.teamSlugs
opts @{config} partialTeamName "list" =
  filter (isPrefixOf partialTeamName) config.teamSlugs

-- finally, everything else (assign, assign --dry) auto-completes with 
-- either a team slug or '+' followed by a user login:
opts @{config} "--" _ = config.teamSlugs
opts @{config} partialTeamName _ =
  filter (isPrefixOf partialTeamName) slugsOrLogins
    where
      -- If the word being typed is prefixed with '+' return user logins
      -- but otherwise return team slugs. 
      slugsOrLogins : List String
      slugsOrLogins =
        if "+" `isPrefixOf` partialTeamName
          then (strCons '+') <$> config.orgMembers
          else config.teamSlugs

export
script : String
script = """
_harmony()
{
  ED=$([ -z $2 ] && echo "--" || echo $2)
  COMPREPLY=($(harmony --bash-completion $ED $3))
}

complete -F _harmony harmony
"""

