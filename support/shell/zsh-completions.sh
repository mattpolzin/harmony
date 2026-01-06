#compdef harmony
compdef _harmony harmony

__harmony_debug()
{
    local file="$ZSH_COMP_DEBUG_FILE"
    if [[ -n ${file} ]]; then
        echo "$*" >> "${file}"
    fi
}

_harmony()
{
  PREV_IDX=$((CURRENT-1))
  __harmony_debug "CURRENT: ${CURRENT}, PREV_IDX: ${PREV_IDX}, words[*]: ${words[*]}, PREFIX: ${PREFIX}"

  CURRENT_PARTIAL=$([[ -z ${PREFIX} ]] && echo "--" || echo "${PREFIX}")
  PREVIOUS="${words[$PREV_IDX]}"
  SUBCOMMAND=$([ -z ${words[2]} ] && echo "--" || echo "${words[2]}")

  __harmony_debug "CURRENT_PARTIAL: ${CURRENT_PARTIAL}, PREVIOUS: ${PREVIOUS}, SUBCOMMAND: ${SUBCOMMAND}"

  REPLY="$(harmony --zsh-completion "$SUBCOMMAND" "$CURRENT_PARTIAL" "$PREVIOUS")"

  __harmony_debug "REPLY: ${REPLY}"

  IFS_OLD="${IFS}"
  IFS='~'
  read -rA comps <<< "${REPLY}"
  IFS="${IFS_OLD}"

  _describe 'harmony' comps
}

# don't run the completion function when being source-ed or eval-ed
if [ "$funcstack[1]" = "_harmony" ]; then
    _harmony
fi
