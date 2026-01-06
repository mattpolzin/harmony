_harmony()
{
  CURRENT_PARTIAL=$([ -z $2 ] && echo "--" || echo "$2")
  PREVIOUS="$3"
  SUBCOMMAND=$([ -z ${COMP_WORDS[1]} ] && echo "--" || echo "${COMP_WORDS[1]}")
  COMPREPLY=($(harmony --bash-completion "$SUBCOMMAND" "$CURRENT_PARTIAL" "$PREVIOUS"))
}

complete -F _harmony harmony
