unsetopt global_rcs

if [ -f "${HOME}/.gpg-agent-info" ]; then
  . "${HOME}/.gpg-agent-info"
  export GPG_AGENT_INFO
  export SSH_AUTH_SOCK
  export SSH_AGENT_PID
else
  gpg-agent --daemon --enable-ssh-support --write-env-file "${HOME}/.gpg-agent-info"
fi
