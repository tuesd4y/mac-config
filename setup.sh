#!/bin/bash

# install xcode cli tools
sudo xcode-select --install

# homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew tap homebrew/cask
brew tap homebrew/cask-fonts
brew tap osgeo/osgeo4mac
brew tap heroku/brew
brew tap homebrew/cask-versions

# general mac setup
brew install git
# for more details on bash installation see here
# https://stackoverflow.com/questions/10574969/how-do-i-install-bash-3-2-25-on-mac-os-x-10-5-8
brew install bash
echo '/usr/local/bin/bash' | sudo tee -a /etc/shells

# zsh shell setup
ln -s ~/config/dotfiles/.zshrc ~/.zshrc
brew install zsh zsh-completions

# dotfiles
ln -s ~/config/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/config/dotfiles/.gitignore ~/.gitignore
ln -s ~/config/dotfiles/.vscode ~/.vscode
ln -s ~/config/dotfiles/karabiner .config/karabiner
ln -s ~/config/dotfiles/env.zsh ~/env.zsh
ln -s ~/config/dotfiles/bitbar ~/.bitbar
ln -s ~/config/dotfiles/.hammerspoon ~/.hammerspoon

# private dotfiles
mkdir ~/config/private

# private top-level gradle
touch ~/config/private/gradle.properties
mkdir ~/.gradle
ln -s ~/config/private/gradle.properties ~/.gradle/gradle.properties

# general file structure
mkdir ~/opt # bigger scripts and extensions

mkdir ~/code # projects and other code stuff
mkdir ~/code/triply
mkdir ~/code/temp
mkdir ~/code/uni
mkdir ~/code/home

# unfortunately we need to sudo this :(
sudo rm -rf ~/Documents
mkdir ~/docs
ln -s ~/docs ~/Documents
mkdir ~/docs/uni
mkdir ~/docs/triply
mkdir ~/docs/personal

mkdir ~/.envs

# Special setup for m1 macs
if [[ $(uname -m) == 'arm64' ]]; then
	# install rosetta which is still needed for some docker features
	softwareupdate --install-rosetta
fi

# tooling stuff
brew install postgresql \
	postgis \
	wget \
	tree \
	fzf \
	docker \
	tldr \
	thefuck \
	tmux \
	jenv \
	imagemagick \
	jq \
	r \
	maven \
	gradle \
	nvm \
	pyenv \
	ripgrep \
	bat \
	tree \
	fswatch \
	ncdu \
	heroku \
	awscli \
	firebase-cli \
	google-cloud-sdk

# apps:
brew install iterm2 \
	jetbrains-toolbox \
	google-chrome \
	spotify \
	qgis \
	visual-studio-code \
	typora \
	iina \
	1password \
	yandex-disk \
	postman \
	karabiner-elements \
	hammerspoon \
	calibre \
	transmission \
	google-drive-file-stream \
	bitbar \
	rstudio

# ql plugins
brew cask install quickgeojson \
	qlcolorcode \
	qlstephen \
	qlmarkdown \
	quicklook-json \
	betterzip # README and other extension-less files`

# use system node version per default
nvm alias default system

# npm packages
npm install -g geojsonio-cli
npm install -g @mapbox/polyline
npm install -g gjv
npm install -g localtunnel
npm install -g md-to-pdf

# configure localtunnel to lct to work with default zsh aliases
mv /usr/local/bin/lt /usr/local/bin/lct
echo '#!/bin/bash' >~/config/private/exports.sh

# configure git ll alias for fzf_git_log function
git config --global alias.ll 'log --graph --format="%C(yellow)%h%C(red)%d%C(reset) - %C(bold green)(%ar)%C(reset) %s %C(blue)<%an>%C(reset)"'

# alfred (without brew)
curl https://cachefly.alfredapp.com/Alfred_3.8.6_972.dmg >alfred.dmg
open alfred.dmg
cp -a /Volumes/Alfred/Alfred\ 3.app /Applications/Alfred.app
# then copy the license key data in there and import settings from old machine.

# jenv config
# might be a different version now
jenv add /Library/Java/JavaVirtualMachines/openjdk-13.0.2.jdk/Contents/Home
jenv add jenv add /Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home/
jenv global 1.8
jenv enable-plugin maven
jenv enable-plugin export

# fonts
brew cask install font-firacode-nerd-font-mono

# login to cli tools
heroku login
aws configure sso
firebase login

# other configuration
# selection in quick-look
defaults write com.apple.finder QLEnableTextSelection -bool TRUE
killall Finder

# change screenshots folder to pictures/screens
mkdir ~/Pictures/screens
defaults write com.apple.screencapture location /Users/dev/Pictures/screens

# python with pyenv + jupyter
pyenv install 3.8.1

pyenv virtualenv 3.8.1 jupyter3
pyenv virtualenv 3.8.1 tools3

pyenv activate jupyter3
pip install jupyter
python -m ipykernel install --user
pyenv deactivate

pyenv local 3.8.1 jupyter3
pyenv global 3.8.1 jupyter3

pip install virtualenv
pip install virtualenvwrapper
