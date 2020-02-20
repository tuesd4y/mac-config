# Install on new mac

 general information: https://sourabhbajaj.com/mac-setup/Homebrew/

first check out this repository by running 

```bash
git clone https://github.com/tuesd4y/mac-config.git ~/config
```



# scripts

```bash
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
```

# system settings

- right click dock, turn on auto-hiding

# apps

https://typora.io

## iterm setup

- in iterm settings `General > Preferences` tick both boxes and set folder to `config/apps/iterm`

- install firaCode font

  ```bash
  brew cask install font-firacode-nerd-font-mono
  ```

- set the font manually in the profile preferences