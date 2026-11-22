module CommandStubs

import Data.List
import Data.String
import Data.Config

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Util
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

public export
data Argument = Required String | Optional String

export
argument : String -> Doc AnsiStyle
argument = annotate (color Green) . pretty

export
pretty : Argument -> Doc AnsiStyle
pretty (Required n) = "{" <+> (argument n) <+> "}"
pretty (Optional n) = "[" <+> (argument n) <+> "]"

HelpParagraphs : Type
HelpParagraphs = List (Doc AnsiStyle)

export
shell : String -> Doc AnsiStyle
shell = annotate italic . pretty

option : String -> Doc AnsiStyle
option = annotate bold . pretty

configHelp : HelpParagraphs
configHelp =
  [ reflow """
      Get or set the value of a configuration property. Not all properties
      can be set and read via this subcommand.
      """
    <+> line
    <+> argument "properties" <+> ":" <++> (align $ concatWith (\a,b => a <+> "," <+> softline <+> b) $ option <$> settablePropNames)
  ]

contributeHelp : HelpParagraphs
contributeHelp =
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

graphHelp : HelpParagraphs
graphHelp =
  [ reflow """
           Graph the relative review workload of the members of the given
           GitHub Team.
           """
  ]

labelHelp : HelpParagraphs
labelHelp =
  [ reflow "Add one or more labels to a PR, creating a new PR if one does not exist."
    <+> line
    <+> reflow "Labels that do not exist yet will be created automatically."
  ]

prHelp : HelpParagraphs
prHelp =
  [ reflow "Identify an existing PR or create a new one for the current branch."
  , reflow "Apply any number of labels by prefixing them with '#'."
  , reflow "Mark a new or existing PR as a draft with the --draft flag."
  , reflow "Mark an existing PR as ready for review with the --ready flag."
  , reflow "Create and link a GitHub Issue with --issue. Jira issue creation is not yet supported."
  , reflow "Associate an existing project with the new issue with --project (only supported when --issue is used)."
  , reflow "Specify the branch to merge into with the --into option."
  , reflow "Print a pr tree with the --print-tree flag."
  , reflow "Output in markdown or shell (default) format."
  ]

quickHelp : HelpParagraphs
quickHelp =
  [
    hcat [ reflow """
                  Quickly spin up new work with a GitHub or Jira issue and associated feature branch.
                  If you have harmony configured to parse GitHub issue numbers in branch names (\
                  """
         ,  shell "harmony config branchParsing github"
         , reflow """
                  ) then any PR created from the new branch will automatically
                  link to the new ticket in its description.
                  """
         ]
  , reflow """
      By default the branch will have the 'feature' prefix. If the
      --bugfix flag is specified, the 'bugfix' prefix will be used.
      """
  , reflow """
      By default the issue created will not be associated with any projects. If you
      specify the --project option with either a project number or project title
      then that project will be looked up and associated with the new issue. You
      cannot create new projects this way; only existing projects are supported.
      """
  , reflow """
      If your only argument (possibly in addition to the bugfix flag) is
      #<issue-number> for an existing GitHub issue or %<jira-slug> for an
      existing Jira issue then the new branch will point at that existing
      issue. Otherwise, all additional arguments other than --bugfix and
      --project will be used as the issue title. If you don't specify the title
      as CLI arguments, you will be prompted to enter the issue title
      interactively. You will also be prompted for the issue description
      interactively regardless.
      """
  , reflow """
      Currently issue creation is limited to GitHub issues, though selecting
      existing issues supports both GitHub and Jira.
      """
  ]

requestHelp : HelpParagraphs
requestHelp = 
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

slowHelp : HelpParagraphs
slowHelp =
  [ reflow """
           set an issue to be the parent of upcoming 'quick' issues and
           optionally stub the slow issue out with a series of child issues right
           away. This is longer term project planning
           """
  ]

public export
record Command where
  constructor C
  name : String
  shortDescription : String
  arguments : List Argument
  paragraphs : List (Doc AnsiStyle)

export
allCommands : List Command
allCommands =
  [ C "branch"
      "print a GitHub URI for the current branch"
      []
      ["Print the GitHub URI for the currently checked out branch."]
  , C "config"
      "get or set configuration options"
      [Optional "<property>", Optional "<value>"]
      configHelp
  , C "contribute"
      "get one or more PRs awaiting your review"
      [ Optional "-c/--checkout | -l/--list"
      , Optional "-<num>"
      , Optional "-i/--ignore {<uri> | <pr-number>}"
      ]
      contributeHelp
  , C "graph"
      "graph data about recent PR review activity for a team"
      [Optional "-c/--completed", Required "<team-slug>"]
      graphHelp
  , C "health"
      "show open PR count by-month"
      []
      [reflow "Graph all open PRs grouped by the month they were created."]
  , C "help"
      "show harmony usage and help"
      [Optional "<subcommand>"]
      ["Print help"]
  , C "label"
      "add one or more labels to a PR for the current branch, creating the PR if needed"
      [Required "<label>", Optional "..."]
      labelHelp
  , C "list"
      "list teams or list team members on a particular team"
      [Optional "<team-slug>"]
      ["List all teams or the members of the given GitHub Team."]
  , C "pr"
      "get the current branch's PR or create one if needed"
      [ Optional "--ready"
      , Optional "--draft"
      , Optional "--issue"
      , Optional "--project {<project-number> | <project-title>}"
      , Optional "-i/--into {<branch-name>}"
      , Optional "--print-tree"
      , Optional "-o/--output {format}"
      , Optional "#<label>"
      , Optional "..."
      ]
      prHelp
  , C "quick"
      "create a new GitHub issue and branch quickly"
      [ Optional "--bugfix"
      , Optional "--project {<project-number> | <project-title>}"
      , Optional "<title> | #<gh-issue-number> | %<jira-slug>"
      , Optional "..."
      ]
      quickHelp
  , C "reflect"
      "show detailed information about your own review activity"
      []
      [reflow "Reflect on the current state of ones own PRs and review requests."]
  , C "request"
      "request review for the current branch (creating a PR if needed)"
      [Required "<team-slug> | +<user-login>", Optional "#<label>", Optional "..."]
      requestHelp
  , C "rq"
      "request review for the current branch (creating a PR if needed)"
      []
      ["Alias for 'request' command."]
  , C "slow"
      "set a parent issue for upcoming 'quick' issues and maybe stub some issues out"
      [Optional "-s/--stub"]
      slowHelp
  , C "sync"
      "pull labels, teams, etc. from GitHub and cache locally"
      []
      ["Synchronize local config with information from GitHub."]
  , C "version"
      "show harmony version information"
      [Optional "-s/--short"]
      ["Print version"]
  , C "whoami"
      "show information about the authenticated user"
      []
      [reflow "Print information about the configured and authenticated user."]
  ]

export
commandNamed : (name : String) -> Maybe Command
commandNamed n =
  find (\c => c.name == n) allCommands

export
longestCommandName : Nat
longestCommandName = 10

longestCommandNamePrf : CommandStubs.longestCommandName === foldr (Prelude.max . String.length . CommandStubs.Command.name) 0 CommandStubs.allCommands
longestCommandNamePrf = Refl

