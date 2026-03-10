###############################
###    general path setup   ###
###############################
export PATH="$PATH:/usr/local/anaconda3"
export PATH="$HOME/.jenv/bin:$PATH"
export WORKON_HOME=~/.envs
export USER_HOME=/Users/dev
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval "$(thefuck --alias)"

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

export PATH="/Users/dev/opt/pdfannots2json:$PATH"

# add config/scripts to path
export PATH="/Users/dev/config/scripts:$PATH"

alias nx="npm run nx --"
alias upload_data="rclone --config rclone.conf copy data proj-data:data --progress --transfers=40 --checkers=40 --tpslimit=10 --drive-chunk-size=1M --max-backlog 200000"
alias download_data="rclone --config rclone.conf copy proj-data:data data --progress --transfers=40 --checkers=40 --tpslimit=10 --drive-chunk-size=1M --max-backlog 200000"
alias yta='youtube-dl --ignore-config -x'

# rust stuff
export PATH="/Users/dev/.cargo/bin:$PATH"

# external docker server
# Replace $DOCKER_SERVER with the hostname or IP address of the server
# export DOCKER_HOST=tcp://173.212.193.241:2376
# export DOCKER_TLS_VERIFY=1
# export DOCKER_CERT_PATH=$HOME/.docker/

# yarn config
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"


source /Users/dev/config/private/exports.sh

# jenv config
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"

# gcloud cli
if [ -f '/Users/dev/opt/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/dev/opt/google-cloud-sdk/path.zsh.inc'; fi
if [ -f '/Users/dev/opt/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/dev/opt/google-cloud-sdk/completion.zsh.inc'; fi

# claude code gcloud config
export CLAUDE_CODE_USE_VERTEX=1  # claude-code-vertex-setup
export CLOUDSDK_ACTIVE_CONFIG_NAME=claude-code  # claude-code-vertex-setup
export CLOUD_ML_REGION=global  # claude-code-vertex-setup
export ANTHROPIC_VERTEX_PROJECT_ID=vertex-ai-489209  # claude-code-vertex-setup
