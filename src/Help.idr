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

"""

