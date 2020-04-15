###############################
###    general path setup   ###
###############################

export PATH="$HOME/.jenv/bin:$PATH"
export WORKON_HOME=~/.envs
export USER_HOME=/Users/dev
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval "$(pyenv init -)"
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi
eval "$(thefuck --alias)"
eval "$(jenv init -)"
. $(brew --prefix asdf)/asdf.sh




###############################
###  optional android stuff ###
###############################

export ANT_HOME=/usr/local/opt/ant
export MAVEN_HOME=/usr/local/opt/maven
export GRADLE_HOME=/usr/local/opt/gradle
export ANDROID_HOME=/usr/local/share/android-sdk
export ANDROID_NDK_HOME=/usr/local/share/android-ndk
export INTEL_HAXM_HOME=/usr/local/Caskroom/intel-haxm

export PATH=$ANT_HOME/bin:$PATH
export PATH=$MAVEN_HOME/bin:$PATH
export PATH=$GRADLE_HOME/bin:$PATH
export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH
export PATH=$ANDROID_HOME/build-tools/29.0.3:$PATH




###############################
### helpful shell functions ###
###############################

# inspired by https://sourabhbajaj.com/mac-setup/iTerm/fzf.html

# fd - cd to selected directory
fzd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# fh - search in your command history and execute selected command
fh() {
  eval $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

# ch - browse chrome history
ch() {
  local cols sep
  cols=$(( COLUMNS / 3 ))
  sep='{::}'

  cp -f ~/Library/Application\ Support/Google/Chrome/Default/History /tmp/h

  sqlite3 -separator $sep /tmp/h \
    "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
  awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
  fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs open
}

# transfer.sh file sharing
transfer() { if [ $# -eq 0 ]; then echo -e "No arguments specified. Usage:\necho transfer /tmp/test.md\ncat /tmp/test.md | transfer test.md"; return 1; fi
tmpfile=$( mktemp -t transferXXX ); if tty -s; then basefile=$(basename "$1" | sed -e 's/[^a-zA-Z0-9._-]/-/g'); curl --progress-bar --upload-file "$1" "https://transfer.sh/$basefile" >> $tmpfile; else curl --progress-bar --upload-file "-" "https://transfer.sh/$1" >> $tmpfile ; fi; cat $tmpfile; rm -f $tmpfile; }

alias ql='qlmanage -p "$@" >& /dev/null'

alias build='./gradlew build'

alias pyg=/usr/local/Cellar/pygments/2.4.2_1/bin/pygmentize
hi() { 
  cat $1 | pyg | less 
}

#---
## Open the passed file with typora 
## (should mainly be used as an alternative for editing markdown files)
#---
tp() {
  open -a Typora "$1"
}

alias grad=./gradlew
alias vg=geojsonio
alias gjm='geojson-merge'


################################
### fixing noisy wget output ###
################################ 

export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_COLLATE=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
