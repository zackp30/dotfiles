unsetopt global_rcs
function detect_command() {
    command -v $1 >/dev/null 2>&1 || return 1
    return 0
}

function source_if_exists() {
    [ ! -e "$1" ] || source "$1"
}

if [ $(detect_command gpg) ]; then
    if [ -f "${HOME}/.gpg-agent-info" ]; then
        . "${HOME}/.gpg-agent-info"
        export GPG_AGENT_INFO
    else
        gpg-agent --daemon --write-env-file "${HOME}/.gpg-agent-info"
    fi
fi

if [ "$HOST" = "burlg" ]; then
    export LANG=en_US.UTF-8
    export LANGUAGE=en_US.UTF-8
fi

source ~/.locals.sh
