export ZSH="$HOME/.config/zsh/oh-my-zsh/.oh-my-zsh"
#ZSH_THEME="alanpeabody"
HISTFILE=~/.cache/zsh/history
plugins=(git)
source $ZSH/oh-my-zsh.sh
source "$XDG_CONFIG_HOME/shell/aliasrc"
#cowsay "$(shuf -n 1 ~/.local/share/vocab/words.txt)"
#PROMPT='[%F{blue}%n%F{red}@%m %f %F{blue}%~%f]$ '
PROMPT="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %f %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "

#function countdown(){
#   date1=$((`date +%s` + $1)); 
#   while [ "$date1" -ge `date +%s` ]; do 
#     echo -ne "$(date -u --date @$(($date1 - `date +%s`)) +%H:%M:%S)\r";
#     sleep 0.1
#   done
#   notify-send "Task Done"
#}
function stopwatch(){
  date1=`date +%s`; 
   while true; do 
    echo -ne "$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)\r"; 
    sleep 0.1
   done
}
# vi mode
bindkey -v
export KEYTIMEOUT=20

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char
#bindkey 'jk' vi-cmd-mode
bindkey -M viins 'jk' vi-cmd-mode
bindkey -M viins 'kj' vi-cmd-mode
#bindkey -v 'jj' vi-cmd-mode


# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[2 q';;      # block
        viins|main) echo -ne '\e[6 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[6 q"
}
zle -N zle-line-init
echo -ne '\e[6 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[6 q' ;} # Use beam shape cursor for each new prompt.


#source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

