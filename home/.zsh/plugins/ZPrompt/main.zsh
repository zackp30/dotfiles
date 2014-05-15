autoload -U colors && colors
function _zprompt_plugin_rvm() {
        _zprompt_plugin_rvm_version="$(rvm list strings)"
}
function _zprompt_util_strip_colour() {
    local stuffthing
    #local var
    #var="$(echo $1 |  sed 's/\(%\(F\|K\){[a-z]\+}\|\(%f\|%k\|%b\|%B\|%{\|%}\)\)//g')"
    stuffthing="$(echo $1 | sed 's/\(%f\|%b\|%k\)//g' | sed 's/\%//g' | sed 's/\(F\|K\){[a-z]\+}//g' | sed 's/\({fb}\|{B}\|{b}\)//g' | sed 's/{}//g')"
    print $stuffthing
}

function _zprompt_plugin_system_load() {
   le_color=$(( $le_color + 1 ))
   #if [[ $toggle == "true" ]] ; then
       #toggle="false"
       #_zprompt_plugin_system_load_status="%F{red}•%f"
   #else
       #toggle="true"
       #_zprompt_plugin_system_load_status="%F{green}•%f"
   #fi
   if [[ $le_color == 1 ]] ; then ;  _zprompt_plugin_system_load_status="%F{red}•%f" ; fi
   if [[ $le_color == 2 ]] ; then ; _zprompt_plugin_system_load_status="%F{green}•%f" ; fi
   if [[ $le_color == 3 ]] ; then ; _zprompt_plugin_system_load_status="%F{yellow}•%f" ; fi
   if [[ $le_color == 4 ]] ; then ; le_color=0 ; fi
}

function _zprompt_plugin_top_bar() {
        if [[ $_zprompt_plugin_top_bar_middle == true ]] ; then
                local var
                _zprompt_plugin_top_bar_status_middle_one=''
                _zprompt_plugin_top_bar_status_middle_two=''
                (( var = ($COLUMNS - $_zprompt_expansion_length)/4 + 1))
                _zprompt_plugin_top_bar_status_middle_one="$_zprompt_plugin_top_bar_status_left_expansion"
                for i in {1..$var}; do
                    _zprompt_plugin_top_bar_status_middle_one+="$_zprompt_plugin_top_bar_status_left_cfg"
                    _zprompt_plugin_top_bar_status_middle_two+="$_zprompt_plugin_top_bar_status_right_cfg"
                done
                _zprompt_plugin_top_bar_status_middle_two+="$_zprompt_plugin_top_bar_status_right_expansion"
        else
                # Handle resizes.
                _zprompt_plugin_top_bar_status=''
                (( var = ($COLUMNS - $_zprompt_expansion_length)/${#$(_zprompt_util_strip_colour $_zprompt_plugin_top_bar_status_cfg)} ))
                for i in {1..$var}; do
                    _zprompt_plugin_top_bar_status+="$_zprompt_plugin_top_bar_status_cfg"
                done
        fi
}
prompt_expand () { print -P "$1" }
function _zprompt_init() {
    _zprompt_misc_hostname_stuff="$(prompt_expand '%n@%m%f')"
    _zprompt_plugin_system_load
    _le_pwd="$(prompt_expand '%~')"
    _zprompt_expansion_two="$_zprompt_plugin_top_bar_status_left_expansion$_zprompt_plugin_top_bar_status_right_expansion"
    _zprompt_expansion="%F{green}{%f$(vcs_super_info)%K{white}%F{black}$(prompt_expand '%*')%k%f%K{red}%F{black}$_le_pwd%k%f%F{green}}%f"
    levar="$(_zprompt_util_strip_colour \"$_zprompt_expansion$_zprompt_expansion_two\")"
    _zprompt_expansion_length=${#levar}
    _zprompt_plugin_top_bar
    _le_var_of_stuff=$levar
}

function precmd() { setopt prompt_subst ; _zprompt_init ;
    if [[ $_zprompt_plugin_top_bar_middle == true ]] ; then
        #PS1="$_zprompt_plugin_top_bar_status_middle_one$_zprompt_expansion$_zprompt_plugin_top_bar_status_middle_two$_zprompt_plugin_system_load_status%F{green}%n@%m%f%# %F{yellow}>>>%f"
        PS1="$_zprompt_plugin_top_bar_status_middle_one$_zprompt_expansion$_zprompt_plugin_top_bar_status_middle_two$_zprompt_plugin_system_load_status >>>"
    else
        PS1="$_zprompt_expansion$_zprompt_plugin_top_bar_status$_zprompt_plugin_system_load_status%F{green}%~ %n@%m%f%# %F{yellow}>>>%f"
    fi
}
