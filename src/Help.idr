module Help

||| The Help string for Harmony.
export
help : String
help = """
harmony <subcommand>

subcommands:
  help
   - Print help
  sync
   - Synchronize local config with information from GitHub.
  pr
   - Identify an existing PR or create a new one for the current branch.
  list <team-slug>
   - List the members of the given GitHub Team.
  assign {<team-slug> | +<user-login>} [...]
   - Assign the given team(s) and one lucky member from one of those teams
     to review the PR for the current branch.
     
     Also assign any users with logins specified. You specify these
     additional users by prefixing their logins with '+'.

bash completion:
  You can set up bash completion by adding the following to your resource
  or bash profile:

    eval "$(harmony --bash-completion-script)"

  Zsh users will also need to have the following in their resource or
  zsh profile before the above eval:

    autoload -U +X compinit && compinit
    autoload -U +X bashcompinit && bashcompinit

"""

