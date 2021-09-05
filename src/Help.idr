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
  list <team-name>
   - List the members of the given GitHub Team.
  assign <team-name>
   - Assign the given team and one lucky member to review the PR for the current branch.

"""

