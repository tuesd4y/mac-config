# install xcode
sudo xcode-select --install

# homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew tap homebrew/cask-fonts
brew tap osgeo/osgeo4mac

# shell setup
ln -s ~/config/dotfiles/.zshrc ~/.zshrc
brew install zsh zsh-completions

# dotfiles
ln -s ~/config/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/config/dotfiles/.gitignore ~/.gitignore
ln -s ~/config/dotfiles/.vscode ~/.vscode
ln -s ~/config/dotfiles/karabiner .config/karabiner
ln -s ~/config/dotfiles/env.zsh ~/env.zsh

# private dotfiles
mkdir ~/config/private

# private top-level gradle
touch ~/config/private/gradle.properties
mkdir ~/.gradle
ln -s ~/config/private/gradle.properties ~/.gradle/gradle.properties

# general file structure
mkdir ~/opt # bigger scripts and extensions
mkdie ~/code # projects and other code stuff

# tooling stuff
brew install postgresql \
	postgis \
	wget \
	tree \
	fzf \
	docker \
	pyenv

# python with pyenv + jupyter
pyenv install 3.8.1
pyenv local 3.8.1
pyenv global 3.8.1
brew install jupyter

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
	skype

# alfred (without brew)
curl https://cachefly.alfredapp.com/Alfred_3.8.6_972.dmg > alfred.dmg
open alfred.dmg
cp -a /Volumes/Alfred/Alfred\ 3.app /Applications/Alfred.app
# then copy the license key data in there and import settings from old machine.

# fonts
brew cask install font-firacode-nerd-font-mono