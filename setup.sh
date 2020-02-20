# install xcode
sudo xcode-select --install

# homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew tap homebrew/cask-fonts

# shell setup
ln -s ~/config/dotfiles/.zshrc ~/.zshrc
brew install zsh zsh-completions

# dotfiles
ln -s ~/config/dotfiles/.gitconfig ~/.gitconfig
ln -s ~/config/dotfiles/.vscode ~/.vscode


# private dotfiles
mkdir ~/config/private

# private top-level gradle
touch ~/config/private/gradle.properties
mkdir ~/.gradle
ln -s ~/config/private/gradle.properties ~/.gradle/gradle.properties

# apps:
brew cask install iterm2 \
	intellij-idea \
	google-chrome \
	spotify \ 
	visual-studio-code \
	typora \
	iina \
	alfred \
	1password

# alfred (without brew)
curl https://cachefly.alfredapp.com/Alfred_3.8.6_972.dmg > alfred.dmg
open alfred.dmg
cp -a /Volumes/Alfred/Alfred\ 3.app /Applications/Alfred.app
# then copy the license key data in there and import settings from old machine.

# fonts
brew cask install font-firacode-nerd-font-mono