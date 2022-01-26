export ZSH="$HOME/.config/zsh/oh-my-zsh/.oh-my-zsh"
ZSH_THEME="alanpeabody"
HISTFILE=~/.cache/zsh/history
plugins=(git)
source $ZSH/oh-my-zsh.sh
source "$XDG_CONFIG_HOME/shell/aliasrc"
#cowsay "$(shuf -n 1 ~/.local/share/vocab/words.txt)"
PROMPT='[%F{blue}%n%F{red}@%m %f%F{blue}%~%f]$ '

function countdown(){
   date1=$((`date +%s` + $1)); 
   while [ "$date1" -ge `date +%s` ]; do 
     echo -ne "$(date -u --date @$(($date1 - `date +%s`)) +%H:%M:%S)\r";
     sleep 0.1
   done
   notify-send "Task Done"
}
function stopwatch(){
  date1=`date +%s`; 
   while true; do 
    echo -ne "$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)\r"; 
    sleep 0.1
   done
}

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

