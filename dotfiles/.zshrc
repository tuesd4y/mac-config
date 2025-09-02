# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export TERM="xterm-256color"
source ~/config/dotfiles/antigen.zsh
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export HOMEBREW_NO_AUTO_UPDATE=1

antigen use oh-my-zsh

antigen bundle brew
antigen bundle osx
antigen bundle common-aliases
antigen bundle catimg
antigen bundle colored-man-pages

antigen bundle git
antigen bundle git-extras
antigen bundle gradle
antigen bundle virtualenv
antigen bundle npm
antigen bundle web-search
# antigen bundle docker

antigen bundle Tarrasch/zsh-autoenv
antigen bundle zsh-users/zsh-autosuggestions

# this has to be the last bundle!
antigen bundle zsh-users/zsh-syntax-highlighting

antigen bundle agkozak/zsh-z

# antigen theme bhilburn/powerlevel9k powerlevel9k
antigen theme romkatv/powerlevel10k

antigen apply

source ~/env.zsh
# old powerlevel config, no longer needed
# source ~/config/dotfiles/powerlevelConfig.zsh



# pymoodle autocompletion (Luis)
# eval "$(register-python-argcomplete pymoodle)"

# added by travis gem
[ ! -s /Users/dev/.travis/travis.sh ] || source /Users/dev/.travis/travis.sh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Replace $DOCKER_SERVER with the hostname or IP address of the server
# export DOCKER_HOST=tcp://173.212.193.241:2376
# export DOCKER_TLS_VERIFY=1
# export DOCKER_CERT_PATH=$HOME/.docker/

export PATH="/Users/dev/.cargo/bin:$PATH"

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
###-begin-npm-completion-###
#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm
#

if type complete &>/dev/null; then
  _npm_completion () {
    local words cword
    if type _get_comp_words_by_ref &>/dev/null; then
      _get_comp_words_by_ref -n = -n @ -n : -w words -i cword
    else
      cword="$COMP_CWORD"
      words=("${COMP_WORDS[@]}")
    fi

    local si="$IFS"
    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$cword" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           npm completion -- "${words[@]}" \
                           2>/dev/null)) || return $?
    IFS="$si"
    if type __ltrim_colon_completions &>/dev/null; then
      __ltrim_colon_completions "${words[cword]}"
    fi
  }
  complete -o default -F _npm_completion npm
elif type compdef &>/dev/null; then
  _npm_completion() {
    local si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 npm completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef _npm_completion npm
elif type compctl &>/dev/null; then
  _npm_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       npm completion -- "${words[@]}" \
                       2>/dev/null)) || return $?
    IFS="$si"
  }
  compctl -K _npm_completion npm
fi
###-end-npm-completion-###

[[ $commands[kubectl] ]] && source <(kubectl completion zsh)
export PATH="${PATH}:${HOME}/.krew/bin"

if [ -d "/opt/homebrew/opt/ruby@3.0/bin" ]; then
  export PATH=/opt/homebrew/opt/ruby@3.0/bin:$PATH
  export PATH=`gem environment gemdir`/bin:$PATH
fi

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /opt/homebrew/bin/terraform terraform
export PATH="/opt/homebrew/opt/curl/bin:$PATH"
autoload -U compinit; compinit
source /opt/homebrew/etc/bash_completion.d/az

change_color_preset() { echo -e "\033]1337;SetColors=preset=$1\a" }
dark() { change_color_preset "rose-pine (1)" }
light() { change_color_preset "rose-pine-dawn (1)" }
source /Users/dev/.config/op/plugins.sh

# nx autocompletion
source /Users/dev/.oh-my-zsh/custom/plugins/nx-completion/nx-completion.plugin.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Created by `pipx` on 2024-02-20 14:19:32
export PATH="$PATH:/Users/dev/.local/bin"

export STORYBOOK_FIGMA_ACCESS_TOKEN=

# bun completions
[ -s "/Users/dev/.bun/_bun" ] && source "/Users/dev/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# doom emacs
export PATH="$PATH:$HOME/.config/emacs/bin"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/dev/mambaforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/dev/mambaforge/etc/profile.d/conda.sh" ]; then
        . "/Users/dev/mambaforge/etc/profile.d/conda.sh"
    else
        export PATH="/Users/dev/mambaforge/bin:$PATH"
    fi
fi
unset __conda_setup

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/dev/.cache/lm-studio/bin"

export PATH="$PATH:/Users/dev/opt/ijhttp"
fpath+=~/.zfunc; autoload -Uz compinit; compinit

zstyle ':completion:*' menu select
export PATH="/Users/dev/.pixi/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/dev/opt/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/dev/opt/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/dev/opt/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/dev/opt/google-cloud-sdk/completion.zsh.inc'; fi

# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"
