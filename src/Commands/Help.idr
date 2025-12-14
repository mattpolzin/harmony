module Commands.Help

import Data.Config
import Data.String

import Util

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Util
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

warning : String -> Doc AnsiStyle
warning = annotate (color Yellow) . pretty

option : String -> Doc AnsiStyle
option = annotate bold . pretty

subcommand' : String -> Doc AnsiStyle
subcommand' = annotate (color Magenta) . pretty

subcommand : String -> (arguments : List (Doc AnsiStyle)) -> (paragraphs : List (Doc AnsiStyle)) -> Doc AnsiStyle
subcommand n xs paras = 
  let name = subcommand' n
      args = hsep xs
      desc = concatWith (\p,q => p <+> line <+> line <+> q) paras
  in  name <++> args <+> line <+> indent 4 desc

argument' : String -> Doc AnsiStyle
argument' = annotate (color Green) . pretty

argument : (required : Bool) -> String -> Doc AnsiStyle
argument req n =
  let name = argument' n
  in if req 
        then "{" <+> name <+> "}"
        else "[" <+> name <+> "]"

heading : String -> (section : Doc AnsiStyle) -> Doc AnsiStyle
heading n section = 
  let name = annotate underline $ pretty n
  in  line <+> name <+> ":" <+> line <+> indent 2 section

bullet : Doc AnsiStyle -> Doc AnsiStyle
bullet item = "-" <++> item

shell : String -> Doc AnsiStyle
shell = annotate italic . pretty

subcommandHelp' : String -> Doc AnsiStyle
subcommandHelp' n@"label" = subcommand n [argument True "<label>", argument False "..."] $
  [ reflow "Add one or more labels to a PR, creating a new PR if one does not exist."
    <+> line
    <+> reflow "Labels that do not exist yet will be created automatically."
  ]
subcommandHelp' n@"request" = subcommand n [argument True "<team-slug> | +<user-login>", argument False "#<label>", argument False "..."] $
  [ reflow """
      Request review from the given team(s) and one lucky member from one
      of those teams to review the PR for the current branch.
      """
  , reflow """
      Also request reviews from any users with logins specified. You specify
      these additional users by prefixing their logins with '+'.
      """
  , reflow "Optionally apply any number of labels by prefixing them with '#'."
  ]
subcommandHelp' n@"config" = subcommand n [argument True "<property>", argument False "<value>"] $
  [ reflow """
      Get or set the value of a configuration property. Not all properties
      can be set and read via this subcommand.
      """
    <+> line
    <+> argument' "properties" <+> ":" <++> (align $ concatWith (\a,b => a <+> "," <+> softline <+> b) $ option <$> settablePropNames)
  ]
subcommandHelp' n@"pr" = subcommand n [argument False "--draft", argument False "-i/--into {<branch-name>}", argument False "#<label>", argument False "..."] $
  [ reflow "Identify an existing PR or create a new one for the current branch."
  , reflow "Optionally apply any number of labels by prefixing them with '#'."
  , reflow "Optionally mark a new or existing PR as a draft with the --draft flag."
  , reflow "Optionally specify the branch to merge into with the --into option."
  ]
subcommandHelp' n@"contribute" = subcommand n [argument False "-c/--checkout | -l/--list", argument False "-<num>", argument False "-i/--ignore {<uri>/<pr-number>}"]
  [ reflow """
      Contribute to an open PR. Prints a URL. Prioritizes PRs you are
      requested to review but will also return other PRs.
      """
  , reflow """
      Use dash followed by a number (e.g. '-3') to skip that number of
      potential PRs and return the next.
      """
  , reflow """
      Specify a PR to ignore (only affects the local Harmony config on
      this machine) if you would like to more permanently skip a potential
      PR. Do this with the '--ignore' option followed by a GitHub URI or
      Pull Request number.
      """
  , reflow """
      Use --list to list multiple PRs instead of just one. This option 
      supports skipping PRs but does not support the checkout option because
      there is no single PR that should be checked out.
      """
  ]
subcommandHelp' n@"graph"   = subcommand n [argument False "-c/--completed", argument True "<team-slug>"]
  [reflow "Graph the relative review workload of the members of the given GitHub Team."]
subcommandHelp' n@"help"    = subcommand n [argument False "<subcommand>"] ["Print help"]
subcommandHelp' n@"version" = subcommand n [] ["Print version"]
subcommandHelp' n@"quick"   = subcommand n [argument False "--bugfix", argument False "<title>", argument False "..."] [
    hcat [ reflow "Quickly spin up new work with a GitHub issue and associated feature branch. If you have harmony configured to parse GitHub issue numbers ("
         ,  shell "harmony config branchParsing github"
         , reflow ") then any PR created from the new branch will automatically link to the new ticket in its description."
         ]
  , reflow """
      By default the branch will have the 'feature' prefix. If the
      --bugfix flag is specified, the 'bugfix' prefix will be used.
      """
  , reflow """
      All additional arguments other than --bugfix will be used as the issue
      title. If you don't give any arguments then you will be prompted to enter
      the issue title interactively. You will also be prompted for the issue
      description.
      """
  ]
subcommandHelp' n@"sync"    = subcommand n [] ["Synchronize local config with information from GitHub."]
subcommandHelp' n@"branch"  = subcommand n [] ["Print the GitHub URI for the currently checked out branch."]
subcommandHelp' n@"whoami"  = subcommand n [] [reflow "Print information about the configured and authenticated user."]
subcommandHelp' n@"reflect" = subcommand n [] [reflow "Reflect on the current state of ones own PRs and review requests."]
subcommandHelp' n@"list"    = subcommand n [argument False "<team-slug>"] ["List all teams or the members of the given GitHub Team."]
subcommandHelp' n@"health"  = subcommand n [] [reflow "Graph all open PRs grouped by the month they were created."]
subcommandHelp' n@"rq"      = subcommand n [] ["Alias for 'request' command."]
subcommandHelp' c           = pretty "Unreconized command: \{c}"

||| Print help for a particular subcommand.
export
subcommandHelp : (useDecorations : Bool) -> (terminalColumns : Nat) -> (subcommand : String) -> String
subcommandHelp useDecorations terminalColumns subcommand =
  let decorate = if useDecorations then id else unAnnotate
  in  renderString . layoutPretty (optionsWithBestWidth terminalColumns) $
        decorate (subcommandHelp' subcommand)

helpDocs : Doc AnsiStyle
helpDocs = vsep
  [ "harmony" <++> subcommand' "<subcommand>"
  , heading "Subcommands" $ vsep
      [ subcommandHelp' "branch"
      , subcommandHelp' "config"
      , subcommandHelp' "contribute"
      , subcommandHelp' "graph"
      , subcommandHelp' "health"
      , subcommandHelp' "help"
      , subcommandHelp' "label"
      , subcommandHelp' "list"
      , subcommandHelp' "pr"
      , subcommandHelp' "quick"
      , subcommandHelp' "reflect"
      , subcommandHelp' "request"
      , subcommandHelp' "rq"
      , subcommandHelp' "sync"
      , subcommandHelp' "version"
      , subcommandHelp' "whoami"
      ]
  , heading "Authentication" $ vsep
    [ reflow """
        Authentication with GitHub uses a Personal Access Token. You can
        configure which token to use with:
        """
    , ""
    , hsep [ shell "harmony config githubPAT"
           , annotate italic $ argument True "token"
           ]
    , ""
    , hsep [ "Alternatively you can set the"
           , argument' "GITHUB_PAT"
           , "or"
           , argument' "GH_TOKEN"
           , "environment variables."
           ]
    , ""
    , reflow """
        Your Personal Access Token should have the following permissions:
        """
    , bullet $ hsep [argument' "repo", "(Full control of private repositories)"]
    , bullet $ hsep [argument' "read:org", "(Read org and team membership, read org projects)"]
    , bullet $ argument' "read:user"
    , bullet $ argument' "user:email"
    , bullet $ argument' "read:discussion"
    , bullet $ hsep [argument' "read:enterprise", "(Read enterprise profile data)"]
    ]
  , heading "Prompt Completion" $ vsep
      [ reflow """
          You can set up bash completion by adding the following to your bashrc
          file or bash profile:
          """
      , ""
      , shell "eval \"$(harmony --bash-completion-script)\""
      , ""
      , reflow """
          You can set up zsh completion by adding the following to your zshrc
          file or zsh profile:
          """
      , ""
      , shell "eval \"$(harmony --zsh-completion-script)\""
      , ""
      ]
  ]

||| The Help string for Harmony.
export
help : (useDecorations : Bool) -> (terminalColumns : Nat) -> String
help useDecorations terminalColumns =
  let decorate = if useDecorations then id else unAnnotate
  in  renderString . layoutPretty (optionsWithBestWidth terminalColumns) $
        decorate helpDocs

