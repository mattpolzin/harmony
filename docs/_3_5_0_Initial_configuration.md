## Initial configuration
For most things harmony does, you need to have a GitHub 
[Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token).

Your Personal Access Token should have the following permissions:
- `repo` (Full control of private repositories)
- `read:org` (Read org and team membership, read org projects)
- `read:user`
- `user:email`
- `read:discussion`
- `read:enterprise` (Read enterprise profile data)

You can either add the PAT to your environment as the `GITHUB_PAT` (or
alternatively `GH_TOKEN`) variable (perhaps exporting it from your shell
resource file or profile) or you can store your PAT in Harmony's config file.
The first time you start Harmony, it will ask you to configure your PAT if you
don't want to use the Environment variable. You only need one of (a) the ENV var
and (b) the config property and the environment variable will take precedence if
you have both set.

The first time you start Harmony in any particular folder, you will be asked to
provide some information about the GitHub repository you are working with. This
information is stored in a file named `harmony.json` in the current working
directory.

Note that the GitHub organization and repository are both slugs, not names.
These are the values you find in a GitHub URL pointing to your repository.
Harmony works with personal repositories but some of Harmony's features are not
available for these repos because they do not have teams or members.
```
$ harmony sync
Creating a new configuration (storing in harmony.json)...

Harmony uses a GitHub Personal Access Token (PAT) to communicate with GitHub.
You can set this via the $GITHUB_PAT or $GH_TOKEN environment variables or a config property.
If you don't set in your config now, you can set later with `harmony config githubPAT abcdefg`.
The ENV var will always take precedence over the config property.

What PAT would you like to set in the config file (ENTER for default: unset)?

What GitHub org would you like to use harmony for (ENTER for default: myorg)?

What repository would you like to use harmony for (ENTER for default: myrepo)?

What GitHub remote repo would you like to use harmony for (ENTER for default: origin)?

Would you like harmony to comment when it requests reviewers? [Y/n] 
Would you like harmony to request team reviews in addition to individuals when it requests reviewers? [Y/n] 
Creating config...
```

