export PATH=$HOME/bin:$PATH
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:$HOME/.local/bin
function detect_command() {
    command -v $1 >/dev/null 2>&1 || return 1
    return 0
}

function source_if_exists() {
    [ ! -e "$1" ] || source "$1"
}

if [ -f "${HOME}/.gpg-agent-info" ]; then
    . "${HOME}/.gpg-agent-info"
    export GPG_AGENT_INFO
else
    gpg-agent --daemon --write-env-file "${HOME}/.gpg-agent-info"
fi

export PATH="$HOME/.rvm/bin:$PATH" # Add RVM to PATH for scripting
source_if_exists ~/.zsh/plugins/opp.zsh/opp.zsh
source_if_exists ~/.zsh/plugins/opp.zsh/opp/*.zsh
source_if_exists ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source_if_exists ~/.zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source_if_exists $HOME/.homesick/repos/homeshick/homeshick.sh
source_if_exists ~/.rubotorc

if [[ ! "$(which pyenv)" =~ "not found" ]] ; then
    eval "$(pyenv init -)"
fi
setopt prompt_subst
autoload -U colors && colors
export EDITOR=e
HISTSIZE=99999999
SAVEHIST=99999999
HISTFILE=~/.zsh_history
setopt histignorealldups sharehistory

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
setopt interactivecomments
if [[ -z "$STY" && -z "$TMUX" ]] && [[ "$TERM" == (xterm|rxvt|konsole)* || -n "$COLORTERM" ]] && [[ "$TERM" != "dumb"  ]]; then
    export TERM='xterm-256color'
fi

zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo
export TMOUT=3600
bindkey -v
mesg n
export KEYTIMEOUT=1
source_if_exists ~/.zsh/plugins/zsh-vcs-prompt/zshrc.sh
ZSH_VCS_PROMPT_ENABLE_CACHING='true'
fpath=(~/.zsh/plugins/zsh-completions/src ~/.zsh/completion $fpath)
export rvmsudo_secure_path=1

VIM_PROMPT="%F{yellow}%F{blue}[%f%F{yellow}N%f%F{blue}]%k%f"
function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/}"
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
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
# if [ -f "${HOME}/.gpg-agent-info" ]; then
#     . "${HOME}/.gpg-agent-info"
#     export GPG_AGENT_INFO
# fi

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

alias fig="showfigfonts | less"

function gi() { curl -L -s https://www.gitignore.io/api/$@ }



mt_load_mods() {
    for i in $(echo *) ; do echo $i | sed 's/^/load_mod_/g' | sed 's/$/ = true/g' ; done >> $1
}

# BEGIN p_rompt

# Colors
_p_color_date=cyan
_p_color_pwd=cyan
_p_color_pwd_fg=red
_p_color_user=white
_p_color_user_fg=black
_p_color_host=white
_p_color_host_fg=black



p_module_privsymbol() {
    if [[ $(print -P "%#") == "#" ]] ; then
        _p_color_user_privsymbol=red
    else
        _p_color_user_privsymbol=blue
    fi
    echo "%F{$_p_color_user_privsymbol}%#%f"
}

p_module_host() {
    echo "%F{$_p_color_host_fg}%K{$_p_color_host}%m%k%f"
}

p_module_user() {
       echo "%F{$_p_color_user_fg}%K{$_p_color_user}%n%F{red}⍟%f%k%f"
}

p_module_pwd() {
    echo "%F{$_p_color_pwd_fg}%K{$_p_color_pwd}%~%k%f"
}

p_module_time() {
    echo '%F{$_p_color_date}%D{%H:%M:%S}%f'
}

p_load() {
    export PS1=$(p_module_pwd)" "
    export PS1=$PS1$(p_module_time)" "
    export PS1=$PS1$(p_module_user)
    export PS1=$PS1$(p_module_host)" "
    export PS1=$PS1$(p_module_privsymbol)" "
    export PS1=$PS1'$(vcs_super_info)'
    export PS1=$PS1$'\n'
    export PS1=$PS1"➤➤➤ "
}

p_load

# END p_rompt


alias pull='git pull'
alias dict="LD_LIBRARY_PATH=/usr/local/lib64 dict" # ¯\_(ツ)_/¯
alias e="emacsclient -c"
alias ..='cd ..'
alias cab='cabal install'
alias :q='exit'
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' # for TRAMP

[[ $DISPLAY == ":0" ]] && unset TMUX

source_if_exists ~/.locals.sh


task

source_if_exists ~/.fzf.zsh
