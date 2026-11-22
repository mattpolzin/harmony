module Commands.Help

import Data.Config
import Data.String

import CommandStubs
import Util

import Text.PrettyPrint.Prettyprinter
import Text.PrettyPrint.Prettyprinter.Util
import Text.PrettyPrint.Prettyprinter.Render.Terminal

%default total

subcommand' : String -> Doc AnsiStyle
subcommand' = annotate (color Magenta) . pretty

subcommand : Command -> Doc AnsiStyle
subcommand command = 
  let name = subcommand' command.name
      args = hsep (pretty <$> command.arguments)
      desc = concatWith (\p,q => p <+> line <+> line <+> q) command.paragraphs
  in  name <++> args <+> line <+> indent 4 desc

heading : String -> (section : Doc AnsiStyle) -> Doc AnsiStyle
heading n section = 
  let name = annotate underline $ pretty n
  in  line <+> name <+> ":" <+> line <+> indent 2 section

bullet : Doc AnsiStyle -> Doc AnsiStyle
bullet item = "-" <++> item

||| Print help for a particular subcommand.
export
subcommandHelp : (useDecorations : Bool) -> (terminalColumns : Nat) -> (subcommand : String) -> Maybe String
subcommandHelp useDecorations terminalColumns command =
  render . subcommand <$> (commandNamed command)
  where
    decorate : Doc ann -> Doc ann
    decorate = if useDecorations then id else unAnnotate

    render : Doc AnsiStyle -> String
    render = renderString . layoutPretty (optionsWithBestWidth terminalColumns) . decorate

briefHelp : Doc AnsiStyle
briefHelp = vsep
  [ "harmony" <++> subcommand' "<subcommand>"
  , heading "Subcommands" . vsep $ subcommandSummary <$> allCommands
  ]
  where
    subcommandSummary : Command -> Doc AnsiStyle
    subcommandSummary cmd =
      let name = cmd.name
          desc = cmd.shortDescription
          nameLength = length name
          idt = longestCommandName `minus` nameLength
      in subcommand' name <++> indent (cast idt) (pretty desc)

helpDocs : Doc AnsiStyle
helpDocs = vsep
  [ "harmony" <++> subcommand' "<subcommand>"
  , heading "Subcommands" . vsep $ subcommand <$> allCommands
  , heading "Authentication" $ vsep
    [ reflow """
        Authentication with GitHub uses a Personal Access Token. You can
        configure which token to use with:
        """
    , ""
    , hsep [ shell "harmony config githubPAT"
           , annotate italic . pretty $ Required "token"
           ]
    , ""
    , hsep [ "Alternatively you can set the"
           , argument "GITHUB_PAT"
           , "or"
           , argument "GH_TOKEN"
           , "environment variables."
           ]
    , ""
    , reflow """
        Your Personal Access Token should have the following permissions:
        """
    , bullet $ hsep [argument "repo", "(Full control of private repositories)"]
    , bullet $ hsep [argument "read:org", "(Read org and team membership, read org projects)"]
    , bullet $ argument "read:project"
    , bullet $ argument "read:user"
    , bullet $ argument "user:email"
    , bullet $ argument "read:discussion"
    , bullet $ hsep [argument "read:enterprise", "(Read enterprise profile data)"]
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
help : (useDecorations : Bool) -> (longform : Bool) -> (terminalColumns : Nat) -> String
help useDecorations longform terminalColumns =
  let decorate = if useDecorations then id else unAnnotate
      docs : Doc AnsiStyle = if longform then helpDocs else briefHelp
  in  renderString . layoutPretty (optionsWithBestWidth terminalColumns) $
        decorate docs
