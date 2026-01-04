module ShellCompletion.Bash

import Data.Config
import Data.List
import Data.Maybe
import Data.String
import Data.Promise

import System.Git

%default total

allRootCmds : List String
allRootCmds = [ "request"
              , "rq"
              , "branch"
              , "config"
              , "contribute"
              , "graph"
              , "health"
              , "help"
              , "label"
              , "list"
              , "pr"
              , "reflect"
              , "sync"
              , "version"
              , "whoami"
              , "quick"
              ]

||| Turn spaces into '+' so that multi-word phrases can be used with tab-completion.
slugify : String -> String
slugify = pack . replaceOn ' ' '◌' . unpack

||| Take a slugified phrase and undo the transformation to get the original phrase back.
public export
unslugify : String -> String
unslugify = pack . replaceOn '◌' ' ' . unpack

hashify : String -> String
hashify = strCons '#'

public export
unhashify : String -> String
unhashify str = case strM str of
                     StrNil => ""
                     (StrCons '#' str') => str'
                     (StrCons '\\' str') =>
                       case strM str' of
                            StrNil => str
                            (StrCons '#' str'') => str''
                            (StrCons _ _) => str
                     (StrCons _ _) => str

namespace TestUnhashify
  test1 : unhashify "" = ""
  test1 = Refl

  test2 : unhashify "\\" = "\\"
  test2 = Refl

  test3 : unhashify "#hello" = "hello"
  test3 = Refl

  test4 : unhashify "\\hello" = "\\hello"
  test4 = Refl

  test5 : unhashify "\\#hello" = "hello"
  test5 = Refl

export
isHashPrefix : String -> Bool
isHashPrefix str =
  ("#" `isPrefixOf` str) || ("\\#" `isPrefixOf` str)

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

-- then the root commands that take no arguments.
-- this just stops autocomplete from populating any suggestions for these
-- commands' arguments.
cmdOpts "sync"    _ _ = Just []
cmdOpts "health"  _ _ = Just []
cmdOpts "--help"  _ _ = Just []
cmdOpts "reflect" _ _ = Just []
cmdOpts "version" _ _ = Just []

-- next subcommands that have options with no configuration requirement:
cmdOpts "help" "--" "help" = Just allRootCmds
cmdOpts "help" partialArg "help" =
  Just $ filter (isPrefixOf partialArg) allRootCmds 

cmdOpts "quick" "-"  "quick" = Just ["--bugfix"]
cmdOpts "quick" "--" "quick" = Just ["--bugfix"]
cmdOpts "quick" partialArg "quick" =
  if partialArg `isPrefixOf` "--"
     then Just ["--bugfix"]
     else if partialArg `isPrefixOf` "--bugfix"
             then Just ["--bugfix"]
             else Just []

cmdOpts "pr" "-"  "pr" = Just ["--ready", "--draft", "--into"]
cmdOpts "pr" "--" "pr" = Just ["--ready", "--draft", "--into"]
cmdOpts "pr" partialBranch "--into" = Nothing -- <- falls through to handle with config below.
cmdOpts "pr" _ "--ready" = Just ["--into"] -- The ready flag does not work with the --draft flag.
cmdOpts "pr" partialArg "pr" =
  if partialArg `isPrefixOf` "--"
     then Just ["--ready", "--draft", "--into"]
     else if partialArg `isPrefixOf` "--ready"
             then Just ["--ready"]
             else if partialArg `isPrefixOf` "--draft"
                     then Just ["--draft"]
                     else if partialArg `isPrefixOf` "--into"
                          then Just ["--into"]
                          else if isHashPrefix partialArg
                              then Nothing -- <- falls through to handle with config below.
                              else Just []
cmdOpts "pr" partialArg "--draft" =
  if partialArg `isPrefixOf` "--"
     then Just ["--into"]
     else if partialArg `isPrefixOf` "--into"
          then Just ["--into"]
          else if isHashPrefix partialArg
              then Nothing -- <- falls through to handle with config below.
              else Just []
cmdOpts "pr" partialArg branchName =
  -- we ignore the branch name, but this means --into has been used and we can
  -- avoid recommending it
  if partialArg `isPrefixOf` "--"
     then Just ["--draft"]
     else if partialArg `isPrefixOf` "--draft"
             then Just ["--draft"]
             else if isHashPrefix partialArg
                 then Nothing -- <- falls through to handle with config below.
                 else Just []

cmdOpts "contribute" "-"  _ = Just ["--checkout", "-c", "--list", "-l", "--ignore", "-i"]
cmdOpts "contribute" "--" _ = Just ["--checkout", "-c", "--list", "-l", "--ignore", "-i"]
cmdOpts "contribute" partialArg _  =
  if partialArg `isPrefixOf` "--checkout"
     then Just ["--checkout"]
     else if partialArg `isPrefixOf` "--ignore"
             then Just ["--ignore"]
             else if partialArg `isPrefixOf` "--list"
                     then Just ["--list"]
                     else Just []

cmdOpts "graph" "--" _ = Nothing
cmdOpts "graph" "-"  _ = Just ["--completed", "-c"]
cmdOpts "graph" partialArg _ =
  if partialArg `isPrefixOf` "--completed"
     then Just ["--completed"]
     else Nothing

-- anything else requires configuration being loaded
cmdOpts _ _ _ = Nothing

optsForPrIntoOption : HasIO io => (partialBranch : String) -> io (Maybe (List String))
optsForPrIntoOption partialBranch = do
  allBranches <- listBranches'
  let matches = case partialBranch of
                     "--" => allBranches
                     _ => List.filter (isPrefixOf partialBranch) allBranches
  pure $ Just matches

export
ffiOpts : HasIO io => (subcommand : String) -> (curWord : String) -> (prevWord : String) -> io (Maybe (List String))
-- pr command (handled partially above, but when head references are specified, handled here)
ffiOpts "pr" partialBranch "-i"     = optsForPrIntoOption partialBranch
ffiOpts "pr" partialBranch "--into" = optsForPrIntoOption partialBranch
ffiOpts _ _ _ = pure Nothing

optsForRequestCmd : Config => String -> List String
optsForRequestCmd @{config} partialArg =
  if partialArg `isPrefixOf` "--dry"
     then ["--dry"]
     else slugsOrLoginsOrLabels
  where
    -- If the word being typed is prefixed with '+' return user logins
    -- but otherwise return team slugs. 
    slugsOrLoginsOrLabels : List String
    slugsOrLoginsOrLabels =
      if "+" `isPrefixOf` partialArg
        then (strCons '+') <$> config.orgMembers
        else if isHashPrefix partialArg
               then hashify . slugify <$> config.repoLabels
               else config.teamSlugs

export
opts : Config => (subcommand : String) -> (curWord : String) -> (prevWord : String) -> List String
-- we assume we are not handling a root command (see @cmdOpts@ which
-- should have already been called).

-- then the config command
opts @{_} "config" "--" "config" = settablePropNames
opts @{_} "config" partialConfigProp "config" = filter (isPrefixOf partialConfigProp) settablePropNames
opts @{_} "config" _ _ = []

-- and the label command
opts @{config} "label" "--" _ = slugify <$> config.repoLabels
opts @{config} "label" partialLabel _ = filter (isPrefixOf partialLabel) $ slugify <$> config.repoLabels

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

-- then pr (handled partially above, but when labels are specified, handled here)
opts @{config} "pr" partialArg _ =
  if isHashPrefix partialArg
     then hashify . slugify <$> config.repoLabels
     else []

-- finally, request auto-completes with 
-- either a team slug or '+' followed by a user login:
opts @{config} "rq"      "--" "rq"      = "--dry" :: config.teamSlugs
opts @{config} "request" "--" "request" = "--dry" :: config.teamSlugs
opts @{config} "rq"      "--" _ = config.teamSlugs
opts @{config} "request" "--" _ = config.teamSlugs
opts           "rq"      partialArg _ =
  optsForRequestCmd partialArg
opts           "request" partialArg _ =
  optsForRequestCmd partialArg

opts _ _ _ = []

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

