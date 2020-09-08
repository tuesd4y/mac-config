# Install on new mac

general information: https://sourabhbajaj.com/mac-setup/Homebrew/

first check out this repository by running

```bash
git clone https://github.com/tuesd4y/mac-config.git ~/config
```

then just run

```bash
~/config/setup.sh
```

to get started.

# system settings

- right click dock, turn on auto-hiding

# apps

## iterm setup

- in iterm settings `General > Preferences` tick both boxes and set folder to `config/apps/iterm`
- Set font to fira code mono with 13pt

## chrome

- just login and get plugins, etc auto sync

## Dash

https://frankfurt.kapeli.com/downloads/v4/Dash.zip

## Docker for MAC

https://download.docker.com/mac/stable/Docker.dmg

# TODO

- predefined window-orders
- vivaldi setup
- 1pass setup
- add chrome / vivaldi plugins in here
- add typora triply theme
- using [rsync](https://linux.die.net/man/1/rsync) to mirror local docs/meetings folder onto [google drive](https://drive.google.com/drive/u/1/)
- should be executed whenever a new .md file is written to the local meetings folder
  - maybe also build md files automatically before copying them
  - if above not possible, map karabiner shortcut for that
- some youtube dl usages
- rsync for mirroring calibre library + movies to usb stick / drive

## TODO - more detailed

https://ytdl-org.github.io/youtube-dl/index.html

https://gist.github.com/nyergler/7056c61174194a9af9b4d5d727f1b566 or similar window management

https://github.com/Tarrasch/zsh-autoenv

https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/osx

https://github.com/mapbox/geojsonio-cli look at the examples at the bottom for some magic

vim + usage for markdown editing

https://github.com/Vonng/Capslock

https://github.com/gpakosz/.tmux
