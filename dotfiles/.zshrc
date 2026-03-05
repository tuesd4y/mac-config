zmodload zsh/zprof

export TERM="xterm-256color"
source ~/config/dotfiles/antigen.zsh
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export ANTIGEN_LOG="/tmp/antigen.log"

antigen use oh-my-zsh
antigen bundle brew
antigen bundle macos
antigen bundle common-aliases
antigen bundle catimg
antigen bundle colored-man-pages

antigen bundle git
antigen bundle gradle
antigen bundle virtualenv
antigen bundle npm
antigen bundle jscutlery/nx-completion --branch=main
antigen bundle web-search
antigen bundle docker
antigen bundle chezmoi

antigen bundle agkozak/zsh-z

antigen bundle zsh-users/zsh-syntax-highlighting

antigen apply


source ~/env.zsh

# 1Password cli
source ~/.config/op/plugins.sh

zstyle ':completion:*' menu select

autoload -U +X bashcompinit && bashcompinit
autoload -Uz compinit
if [ "$(date +'%j')" != "$(stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)" ]; then
    compinit
else
    compinit -C
fi

eval "$(starship init zsh)"

zprof