# install xcode
sudo xcode-select --install

# homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew tap homebrew/cask-fonts
brew tap osgeo/osgeo4mac
brew tap heroku/brew
brew tap homebrew/cask-versions

# shell setup
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

# tooling stuff
brew install postgresql \
	postgis \
	wget \
	tree \
	fzf \
	docker \
	pyenv \
	pyenv-virtualenv \
	tldr \
	thefuck \
	tmux \
	firebase-cli \
	heroku \
	awscli \
	jenv \
	pgcli \
	imagemagick

# apps:
brew cask install iterm2 \
	intellij-idea \
	google-chrome \
	spotify \
	visual-studio-code \
	typora \
	iina \
	alfred \
	1password \
	yandex-disk \
	postman \
	karabiner-elements \
	qgis \
	hammerspoon \
	adobe-creative-cloud \
	calibre \
	transmission \
	skype \
	google-drive-file-stream \
	bitbar \
	brew cask install adoptopenjdk8

# ql plugins
brew cask install quickgeojson \
	qlcolorcode `#syntax highlighting` \
	qlstephen `# README and other extension-less files` \
	qlmarkdown \
	betterzip 

# npm packages
npm install -g geojson-cli

# alfred (without brew)
curl https://cachefly.alfredapp.com/Alfred_3.8.6_972.dmg > alfred.dmg
open alfred.dmg
cp -a /Volumes/Alfred/Alfred\ 3.app /Applications/Alfred.app
# then copy the license key data in there and import settings from old machine.

# jenv config
# might be a different version now
jenv add /Library/Java/JavaVirtualMachines/openjdk-13.0.2.jdk/Contents/Home
jenv add jenv add /Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home/
jenv global 13.0.2

# fonts
brew cask install font-firacode-nerd-font-mono

# login to cli tools
heroku login
aws configure sso
firebase login

# other configuration
# selection in quick-look
defaults write com.apple.finder QLEnableTextSelection -bool TRUE; killall Finder

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