module Help

import Data.Config
import Data.String

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

||| The Help string for Harmony.
export
help : (decorated : Bool) -> String
help decorated = """
harmony \{subcommand "<subcommand>"}

\{heading "Subcommands"}:
  \{subcommand "help"}
   - Print help
  \{subcommand "version"}
   - Print version
  \{subcommand "config"} {\{argument "<property>"}} [\{argument "<value>"}]
   - Get or set the value of a configuration property. Not all properties
     can be set and read via this subcommand.
     \{argument "properties"}: \{join ", " $ option <$> settablePropNames}.
  \{subcommand "sync"}
   - Synchronize local config with information from GitHub.
  \{subcommand "branch"}
   - Print the GitHub URI for the currently checked out branch.
  \{subcommand "pr"} [\{argument "--draft"}] [\{argument "#<label>"}] [...]
   - Identify an existing PR or create a new one for the current branch.
     
     Optionally apply any number of labels by prefixing them with '#'.

  \{subcommand "contribute"} [\{argument "-c/--checkout"}] [\{argument "-<num>"}] [\{argument "-i/--ignore"} {\{argument "<uri>/<pr-number>"}}]
   - Contribute to an open PR. Prints a URL. Prioritizes PRs you are
     requested to review but will also return other PRs.

     Use dash followed by a number (e.g. '-3') to skip that number of
     potential PRs and return the next.

     Specify a PR to ignore (only affects the local Harmony config on
     this machine) if you would like to more permanently skip a potential
     PR. Do this with the '--ignore' option followed by a GitHub URI or
     Pull Request number.
  \{subcommand "whoami"}
   - Print information about the configured and authenticated user.
  \{subcommand "reflect"}
   - Reflect on the current state of ones own PRs and review requests.
  \{subcommand "list"} {\{argument "<team-slug>"}}
   - List the members of the given GitHub Team.
  \{subcommand "graph"} [\{argument "-c/--completed"}] {\{argument "<team-slug>"}}
   - Graph the relative review workload of the members of the given GitHub Team.
  \{subcommand "health"}
   - Graph all open PRs grouped by the month they were created.
  \{subcommand "label"} {\{argument "<label>"}} [...]
   - Add one or more labels to a PR, creating a new PR if one does not exist.
     Labels that do not exist yet will be created automatically.
  \{subcommand "request"} {\{argument "<team-slug>"} | \{argument "+<user-login>"}} [\{argument "#<label>"}] [...]
   - Request review from the given team(s) and one lucky member from one
     of those teams to review the PR for the current branch.
     
     Also request reviews from any users with logins specified. You specify
     these additional users by prefixing their logins with '+'.

     Optionally apply any number of labels by prefixing them with '#'.
     
\{heading "Bash Completion"}:
  You can set up bash completion by adding the following to your resource
  or bash profile:
    
    \{shell "eval \"$(harmony --bash-completion-script)\""}
    
  Zsh users will also need to have the following in their resource or
  zsh profile before the above eval:
    
    \{shell "autoload -U +X compinit && compinit"}
    \{shell "autoload -U +X bashcompinit && bashcompinit"}
    
"""
  where
    maybeDecorate : (String -> Doc AnsiStyle) -> String -> String
    maybeDecorate f s = if decorated then renderString . layoutPretty defaultLayoutOptions $ f s else s

    option : String -> String
    option = maybeDecorate (annotate bold . pretty)
    
    subcommand : String -> String
    subcommand = maybeDecorate (annotate (color Magenta) . pretty) 

    argument : String -> String
    argument = maybeDecorate (annotate (color Green) . pretty)

    heading : String -> String
    heading = maybeDecorate (annotate underline . pretty)

    shell : String -> String
    shell = maybeDecorate (annotate italic . pretty)

