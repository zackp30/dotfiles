export PATH=$HOME/bin:$PATH # weird how this isn't a default...
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

# From the ZSH wiki
# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup key accordingly
[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
function zle-line-init () {
    echoti smkx
}
function zle-line-finish () {
    echoti rmkx
}

zle -N zle-line-init
zle -N zle-line-finish

source_if_exists ~/.zsh/plugins/opp.zsh/opp.zsh
source_if_exists ~/.zsh/plugins/opp.zsh/opp/*.zsh
source_if_exists ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source_if_exists ~/.zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source_if_exists $HOME/.homesick/repos/homeshick/homeshick.sh

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

[[ -s /home/zack/.autojump/etc/profile.d/autojump.sh ]] && source /home/zack/.autojump/etc/profile.d/autojump.sh

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

bindkey -v
bindkey -s '^O' '^qcd\n'
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

# fstage - stage uncommited file

fstage() {
    local files to_stage
    files="$(git status --porcelain)"
    to_stage=$(echo $files | fzf -m | rev | cut -d' ' -f1 | rev)
    git add $(echo $to_stage | tr '\n' ' ')
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
    export PS1=$PS1"$ "
}

p_load

alias pull='git pull'
alias ..='cd ..'
alias cab='cabal install'
alias :q='exit'

# `$TERM' is set to "dumb" when using TRAMP to connect to the host, my custom prompt
# doesn't work well with TRAMP (i.e: makes TRAMP wait forever for a prompt), so simply
# set `PS1' to a very simple prompt.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

[[ $DISPLAY == ":2" ]] && unset TMUX

# When in tmux I leave some panes to idle, but when I'm not in tmux I don't need `$TMOUT'
# especially when forwarding ports, so determine if within `TMUX' and set `TMOUT'.
[[ $TMUX ]] && export TMOUT=3600

source_if_exists ~/.locals.sh # host specific things *not* to be checked into version control.

alias gcem="git commit -am '' --allow-empty-message"
alias grmv="git remote -v"

task

source_if_exists ~/.fzf.zsh

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
