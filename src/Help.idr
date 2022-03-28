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
  \{subcommand "config"} {\{argument "<property>"}} [\{argument "value"}]
   - Get or set the value of a configuration property. Not all properties
     can be set and read via this subcommand.
     \{argument "properties"}: \{join ", " settableProps}.
  \{subcommand "sync"}
   - Synchronize local config with information from GitHub.
  \{subcommand "pr"}
   - Identify an existing PR or create a new one for the current branch.
  \{subcommand "contribute"}
   - Contribute to an open PR. Prints a URL. Prioritizes PRs you are
     requested to review but will also return other PRs.
  \{subcommand "reflect"}
   - Reflect on the current state of ones own PRs and review requests.
  \{subcommand "list"} {\{argument "<team-slug>"}}
   - List the members of the given GitHub Team.
  \{subcommand "graph"} {\{argument "<team-slug>"}}
   - Graph the relative review workload of the members of the given GitHub Team.
  \{subcommand "assign"} {\{argument "<team-slug>"} | \{argument "+<user-login>"}} [...]
   - Assign the given team(s) and one lucky member from one of those teams
     to review the PR for the current branch.
     
     Also assign any users with logins specified. You specify these
     additional users by prefixing their logins with '+'.
     
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
    
    subcommand : String -> String
    subcommand = maybeDecorate (annotate (color Magenta) . pretty) 

    argument : String -> String
    argument = maybeDecorate (annotate (color Blue) . pretty)

    heading : String -> String
    heading = maybeDecorate (annotate underline . pretty)

    shell : String -> String
    shell = maybeDecorate (annotate italic . pretty)

