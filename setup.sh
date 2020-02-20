# install xcode
sudo xcode-select --install

# homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew tap homebrew/cask-fonts

# shell setup
ln -s ~/config/dotfiles/.zshrc ~/.zshrc
ln -s ~/config/dotfiles/.vscode ~/.vscode
brew install zsh zsh-completions


# apps:
brew cask install iterm2 \
	intellij-idea \
	google-chrome \
	spotify \ 
	visual-studio-code \
	typora

# fonts
brew cask install font-firacode-nerd-font-mono