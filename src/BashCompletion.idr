module BashCompletion

import Data.Config
import Data.List
import Data.String

import Debug.Trace

%default total

allRootCmds : List String
allRootCmds = [
    "assign"
  , "config"
  , "contribute"
  , "graph"
  , "help"
  , "list"
  , "pr"
  , "reflect"
  , "sync"
  , "version"
  ]

||| Attempt to handle completions for root commands but
||| if we ar not currently on the root command (at least
||| one argument has already been entered), we return
||| @Nothing@ so that code can call out to the full @opts@
||| function after loading the config file.
export
cmdOpts : String -> String -> Maybe (List String)
-- first the root commands:
cmdOpts "--"       "harmony" = Just allRootCmds
cmdOpts partialCmd "harmony" = Just $ filter (isPrefixOf partialCmd) allRootCmds

-- then the subcommands that take no arguments;
cmdOpts _ "pr"         = Just []
cmdOpts _ "sync"       = Just []
cmdOpts _ "help"       = Just []
cmdOpts _ "--help"     = Just []
cmdOpts _ "reflect"    = Just []
cmdOpts _ "version"    = Just []

-- next subcommands that have options with no configuration requirement:
cmdOpts "-" "contribute"  = Just ["--checkout", "-c"]
cmdOpts "--" "contribute" = Just ["--checkout", "-c"]
cmdOpts partialArg "contribute" =
  if partialArg `isPrefixOf` "--checkout"
     then Just ["--checkout"]
     else Just []

-- anything else requires configuration being loaded
cmdOpts _ _ = Nothing

-- given a pair of strings, the first representing the word
-- actually being edited, the second representing the word
-- before the one being edited, return a list of possible
-- completions. If the list of completions is empty, bash
-- will perform directory completion.
export
opts : Config => String -> String -> List String
-- we assume we are not handling a root command (see @cmdOpts@ which
-- should have already been called).

-- then the config command
opts @{_} "--" "config" = settableProps
opts @{_} partialConfigProp "config" = filter (isPrefixOf partialConfigProp) settableProps

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
  ED=$([ -z $2 ] && echo "--" || echo "$2")
  COMPREPLY=($(harmony --bash-completion "$ED" "$3"))
}

complete -F _harmony harmony
"""

