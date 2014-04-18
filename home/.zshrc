if [ -e "$HOME/.envirius/nv" ] ; then
  . ~/.envirius/nv
fi

if [ -e ".envirius" ] && [ -f ".envirius" ]; then
  nv on `cat .envirius`
fi
source ~/.fzf.zsh
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
    source ~/.zshrc.aliases
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
export PATH=$HOME/bin:$PATH:$GOROOT/bin:$GOPATH/bin
.  /usr/share/autojump/autojump.sh 

