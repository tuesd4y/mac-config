export TERM="xterm-256color"
source ~/config/dotfiles/antigen.zsh

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
antigen bundle docker

antigen bundle Tarrasch/zsh-autoenv
antigen bundle zsh-users/zsh-autosuggestions

# this has to be the last bundle!
antigen bundle zsh-users/zsh-syntax-highlighting

antigen theme bhilburn/powerlevel9k powerlevel9k

antigen apply

source ~/env.zsh
source ~/config/dotfiles/powerlevelConfig.zsh
