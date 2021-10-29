export TERM="xterm-256color"
source ~/config/dotfiles/antigen.zsh
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

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

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/dev/anaconda3/bin/conda' 'shell.zsh' 'hook' 2>/dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/dev/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/dev/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/dev/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# pymoodle autocompletion (Luis)
eval "$(register-python-argcomplete pymoodle)"

# added by travis gem
[ ! -s /Users/dev/.travis/travis.sh ] || source /Users/dev/.travis/travis.sh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Replace $DOCKER_SERVER with the hostname or IP address of the server
export DOCKER_HOST=tcp://173.212.193.241:2376
export DOCKER_TLS_VERIFY=1
export DOCKER_CERT_PATH=$HOME/.docker/
