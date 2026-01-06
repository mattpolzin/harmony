module ShellCompletion.Zsh

import Language.Reflection

%default total

%language ElabReflection

||| Zsh completion is performed via Harmony's Bash completion support.
||| For details on the expected structure of bash completion calls, see
||| the docs for the `ShellCompletion.Bash.script` function.
export
script : String
script = %runElab do
  Just completionScript <- readFile ProjectDir "support/shell/zsh-completions.sh"
    | Nothing => 
        fail """
             support/shell/zsh-completions.sh file \
             is missing from the project directory!
             """
  pure completionScript
