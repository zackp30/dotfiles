source $HOME/.homesick/repos/homeshick/homeshick.sh
export PATH=$HOME/bin:$PATH:$GOROOT/bin:$GOPATH/bin:/usr/local/texlive/2013/bin/x86_64-linux:$HOME/.cabal/bin:/usr/local/bin/:$HOME/.pyenv/bin
if [ -e "$HOME/.envirius/nv" ] ; then
  . ~/.envirius/nv
fi

eval "$(pyenv init -)"
if [ -e ".envirius" ] && [ -f ".envirius" ]; then
  nv on `cat .envirius`
fi
setopt prompt_subst
# Prompts {{{
    # Misc {{{
        autoload -U colors && colors
    # }}}
    # Used prompts {{{
    # OR IT COULD BE SOMEWHERE OVER HERE? :D?
    #PS1='%F{red}%K{cyan}%n%K{green}%F{black}@%F{red}%K{blue}%m%F{yellow}%~%F %b$(vcs_super_info)%b %F{black}%K{cyan}%#%f%k '
    # }}}
# }}}
# Editor {{{
    export EDITOR=/usr/local/bin/vim
# }}}
# History {{{
    HISTSIZE=5000
    SAVEHIST=5000
    HISTFILE=~/.zsh_history
    setopt histignorealldups sharehistory
# }}}
# Completion {{{

autoload -Uz compinit
compinit
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
#zstyle ':completion:*' completer _oldlist _complete
# }}}
# Misc {{{
    setopt interactivecomments
    if [[ -z "$STY" && -z "$TMUX" ]] && [[ "$TERM" == (xterm|rxvt|konsole)* || -n "$COLORTERM" ]]; then
        export TERM='xterm-256color'
    fi

    alias fig="showfigfonts | less"

    insert_sudo () { zle beginning-of-line; zle -U "sudo " }
    zle -N insert-sudo insert_sudo
    bindkey "^[s" insert-sudo
    TMOUT=3600
    bindkey -v
    mesg n
    export KEYTIMEOUT=1
# }}}
# Plugins {{{
    . $HOME/.zsh/plugins/bd/bd.zsh
    . ~/.zsh/plugins/zsh-vcs-prompt/zshrc.sh
    bindkey -e
    bindkey $'\e' vi-cmd-mode # From https://github.com/hchbaw/auto-fu.zsh/issues/29
    # . ~/.zsh/plugins/auto-fu.zsh/auto-fu.zsh
    . ~/.zsh/plugins/tmuxinator.zsh
    ZSH_VCS_PROMPT_ENABLE_CACHING='true'
    fpath=(~/.zsh/plugins/zsh-completions/src ~/.zsh/completion $fpath)
    export rvmsudo_secure_path=1

    #export PAGER=~/bin/vimpager
    #alias less=$PAGER
    #alias zless=$PAGER
# }}}
# Vi stuff. {{{

VIM_PROMPT="%F{yellow}%F{blue}[%f%F{yellow}N%f%F{blue}]%k%f"
function zle-line-init zle-keymap-select {
  RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/}"
  zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

source ~/.zsh/plugins/opp.zsh/opp.zsh
source ~/.zsh/plugins/opp.zsh/opp/*.zsh
# }}}
# Stuff that needs to go last {{{
source ~/.zsh/plugins/ZPrompt/main.zsh
source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
# }}}
_zprompt_plugin_top_bar_middle=true
_zprompt_plugin_top_bar_status_cfg='%F{yellow}──%f'
_zprompt_plugin_top_bar_status_left_cfg="%F{yellow}─%f%F{green}─%f"
_zprompt_plugin_top_bar_status_right_cfg="%F{yellow}─%f%F{green}─%f"
_hostname_stuff="$(prompt_expand '%n@%m')"
_zprompt_plugin_top_bar_status_left_expansion="%F{green}$_hostname_stuff%f"
fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
    zle redisplay
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z
autoload -Uz compinit
compinit
export GOROOT=$HOME/go
export GOPATH=$HOME/gostuff
.  /usr/share/autojump/autojump.sh 
if [ -f "${HOME}/.gpg-agent-info" ]; then
  . "${HOME}/.gpg-agent-info"
  export GPG_AGENT_INFO
  export SSH_AUTH_SOCK
  export SSH_AGENT_PID
fi
GPG_TTY=$(tty)
export GPG_TTY

# fzf {{{
# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  local file
  file=$(fzf --query="$1" --select-1 --exit-0)
  [ -n "$file" ] && ${EDITOR:-vim} "$file"
}

# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-*} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# fda - including hidden directories
fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# fh - repeat history
fh() {
  eval $(([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s | sed 's/ *[0-9]* *//')
}

# fkill - kill process
fkill() {
  ps -ef | sed 1d | fzf -m | awk '{print $2}' | xargs kill -${1:-9}
}

# fbr - checkout git branch
fbr() {
  local branches branch
  branches=$(git branch) &&
  branch=$(echo "$branches" | fzf +s +m) &&
  git checkout $(echo "$branch" | sed "s/.* //")
}

# fco - checkout git commit
fco() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

# ftags - search ctags
ftags() {
  local line
  [ -e tags ] &&
  line=$(
    awk 'BEGIN { FS="\t" } !/^!/ {print toupper($4)"\t"$1"\t"$2"\t"$3}' tags |
    cut -c1-80 | fzf --nth=1,2
  ) && $EDITOR $(cut -f3 <<< "$line") -c "set nocst" \
                                      -c "silent tag $(cut -f2 <<< "$line")"
}
# }}}
source ~/.fzf.zsh
