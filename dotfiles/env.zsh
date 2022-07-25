###############################
###    general path setup   ###
###############################
export PATH="$PATH:/usr/local/anaconda3"
export PATH="$HOME/.jenv/bin:$PATH"
export WORKON_HOME=~/.envs
export USER_HOME=/Users/dev
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval "$(thefuck --alias)"
eval "$(jenv init -)"

# node version manager
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

# sphinx doc
export PATH="/usr/local/opt/sphinx-doc/bin:$PATH"
export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig:/usr/local/opt/openexr/lib/pkgconfig:/usr/local/opt/imath/lib/pkgconfig:$PKG_CONFIG_PATH"

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
    -o -type d -print 2>/dev/null | fzf +m) &&
    cd "$dir"
}

# fh - search in your command history and execute selected command
fh() {
  eval $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//')
}

# ch - browse chrome history
ch() {
  local cols sep
  cols=$((COLUMNS / 3))
  sep='{::}'

  cp -f ~/Library/Application\ Support/Google/Chrome/Default/History /tmp/h

  sqlite3 -separator $sep /tmp/h \
    "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
    awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
    fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs open
}

##############################
## SOME MORE FUN WITH FZF ###
##############################

# blatantly stolen from https://bluz71.github.io/2018/11/26/fuzzy-finding-in-bash-with-fzf.html
fzf_grep_edit() {
  if [[ $# == 0 ]]; then
    echo 'Error: search term was not provided.'
    return
  fi
  local match=$(
    rg --color=never --line-number "$1" |
      fzf --no-multi --delimiter : \
        --preview "bat --color=always --line-range {2}: {1}"
  )
  local file=$(echo "$match" | cut -d':' -f1)
  if [[ -n $file ]]; then
    if [[ $TYPORA == 1 ]]; then
      open -a Typora "$file"
    else
      $EDITOR "$file" +$(echo "$match" | cut -d':' -f2)
    fi
  fi
}

alias fge='fzf_grep_edit'

# search for md files in meetings folder with given content and open chosen with typora
meetings() {
  cd ~/docs/meetings/*.md
  TYPORA="1"
  echo "$1"
  fzf_grep_edit "$1"
  cd -
}

# browse git logs
fzf_git_log() {
  local selections=$(
    git ll --color=always "$@" |
      fzf --ansi --no-sort --no-height \
        --preview "echo {} | grep -o '[a-f0-9]\{7\}' | head -1 |
                       xargs -I@ sh -c 'git show --color=always @'"
  )
  if [[ -n $selections ]]; then
    local commits=$(echo "$selections" | cut -d' ' -f2)
    git show $commits
  fi
}
alias gll='fzf_git_log'

# transfer.sh file sharing
transfer() {
  if [ $# -eq 0 ]; then
    echo -e "No arguments specified. Usage:\necho transfer /tmp/test.md\ncat /tmp/test.md | transfer test.md"
    return 1
  fi
  tmpfile=$(mktemp -t transferXXX)
  if tty -s; then
    basefile=$(basename "$1" | sed -e 's/[^a-zA-Z0-9._-]/-/g')
    curl -w "\n" --progress-bar --upload-file "$1" "https://transfer.sh/$basefile" >>$tmpfile
  else curl --progress-bar --upload-file "-" "https://transfer.sh/$1" >>$tmpfile; fi
  cat $tmpfile
  rm -f $tmpfile
}

#---
#Useful aliases:
## Open the passed file with typora
## (should mainly be used as an alternative for editing markdown files)
#---
alias tp="open -a Typora"
alias grad=./gradlew
alias vg=geojsonio
alias gjm='geojson-merge'
alias rstudio='open -a RStudio'
alias ql='qlmanage -p "$@" >& /dev/null'
alias build='./gradlew build'
alias pyg=/usr/local/Cellar/pygments/2.4.2_1/bin/pygmentize

alias pk=pagekite.py

hi() {
  cat $1 | pyg | less
}

################################
### fixing noisy wget output ###
################################

export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_COLLATE=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8

############################
# R and gettext path setup #
############################

export PATH="/usr/local/opt/gettext/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/gettext/lib"
export CPPFLAGS="-I/usr/local/opt/gettext/include"

# add config/scripts to path
export PATH="/Users/dev/config/scripts:$PATH"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# Custom SERVER_URL setup script
# source ~/config/private/export.sh

# setup ruby from asdf on path
. $(brew --prefix asdf)/asdf.sh

alias yta='youtube-dl --ignore-config -x'
source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc
source /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc

# setup for rupa/z
. /usr/local/etc/profile.d/z.sh
