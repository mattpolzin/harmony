module CommandStubs

%default total

public export
record Command where
  constructor C
  name : String
  shortDescription : String

export
allCommands : List Command
allCommands =
  [ C "branch"
      "print a GitHub URI for the current branch"
  , C "config"
      "get or set configuration options"
  , C "contribute"
      "get one or more PRs awaiting your review"
  , C "graph"
      "graph data about recent PR review activity for a team"
  , C "health"
      "show open PR count by-month"
  , C "help"
      "show harmony usage and help"
  , C "label"
      "add one or more labels to a PR for the current branch, creating the PR if needed"
  , C "list"
      "list teams or list team members on a particular team"
  , C "pr"
      "get the current branch's PR or create one if needed"
  , C "quick"
      "create a new GitHub issue and branch quickly"
  , C "reflect"
      "show detailed information about your own review activity"
  , C "request"
      "request review for the current branch (creating a PR if needed)"
  , C "rq"
      "request review for the current branch (creating a PR if needed)"
  , C "slow"
      "set a parent issue for upcoming 'quick' issues and maybe stub some issues out"
  , C "sync"
      "pull labels, teams, etc. from GitHub and cache locally"
  , C "version"
      "show harmony version information"
  , C "whoami"
      "show information about the authenticated user"
  ]

export
longestCommandName : Nat
longestCommandName = 10

longestCommandNamePrf : CommandStubs.longestCommandName === foldr (Prelude.max . String.length . CommandStubs.Command.name) 0 CommandStubs.allCommands
longestCommandNamePrf = Refl

