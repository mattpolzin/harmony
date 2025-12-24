
module ZshCompletion

import BashCompletion

%default total

||| Zsh completion is performed via Harmony's Bash completion support.
||| For details on the expected structure of bash completion calls, see
||| the docs for the `BashCompletion.script` function.
export
script : String
script = """
# compdef harmony
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit

\{BashCompletion.script}
"""

