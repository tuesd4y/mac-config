source ~/config/dotfiles/antigen.zsh

antigen use oh-my-zsh
antigen bundle brew
antigen bundle git
antigen bundle zsh-users/zsh-autosuggestions

# this has to be the last bundle!
antigen bundle zsh-users/zsh-syntax-highlighting

antigen theme bhilburn/powerlevel9k powerlevel9k

antigen apply

source ~/env.zsh