# dotfiles ------------------------------------------------------------------- #

# TODO Clone repo

git clone --bare git@github.com:jesse-c/dots.git $HOME/.dots.git

dots config --local status.showUntrackedFiles no

# Shell ---------------------------------------------------------------------- #

git clone --recursive https://github.com/Eriner/zim.git ${ZDOTDIR:-${HOME}}/.zim

setopt EXTENDED_GLOB
for template_file ( ${ZDOTDIR:-${HOME}}/.zim/templates/* ); do
  user_file="${ZDOTDIR:-${HOME}}/.${template_file:t}"
  touch ${user_file}
  ( print -rn "$(<${template_file})$(<${user_file})" >! ${user_file} ) 2>/dev/null
done

chsh -s =zsh

source ${ZDOTDIR:-${HOME}}/.zlogin # TODO Open new shell first

ln -s ~/.config/zsh/themes/mix.zsh-theme ~/.zim/modules/prompt/themes/mix.zsh-theme
ln -s ~/.zim/modules/prompt/themes/mix.zsh-theme ~/.zim/modules/prompt/functions/prompt_mix_setup

# Homebrew ------------------------------------------------------------------- #

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew bundle

# fzf ------------------------------------------------------------------------ #

$(brew --prefix)/opt/fzf/install

# Languages ------------------------------------------------------------------ #

curl https://sh.rustup.rs -sSf | sh

git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.4.2

asdf plugin-add erlang https://github.com/asdf-vm/asdf-erlang.git
asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git
asdf plugin-add clojure https://github.com/vic/asdf-clojure.git
asdf plugin-add elm https://github.com/vic/asdf-elm.git
asdf plugin-add racket https://github.com/vic/asdf-racket.git
asdf plugin-add golang https://github.com/kennyp/asdf-golang.git
asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf plugin-add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf plugin-add ocaml https://github.com/vic/asdf-ocaml.git
asdf plugin-add lua https://github.com/Stratus3D/asdf-lua.git
asdf plugin-add scala https://github.com/mtatheonly/asdf-scala
asdf plugin-add python https://github.com/tuvistavie/asdf-python.git
asdf plugin-add java https://github.com/skotchpine/asdf-java

asdf install

# Tmux ----------------------------------------------------------------------- #

git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Python --------------------------------------------------------------------- #

pip3 install awscli flake8 neovim yamllint pync websocket-client sqlparse bandit pgcli jedi pycscope

# Ruby ----------------------------------------------------------------------- #

gem install bundler neovim rubocop rerun sqlint scss_lint starscope

# Lua ------------------------------------------------------------------------ #
luarocks install luacheck

# Node ----------------------------------------------------------------------- #

npm install -g eslint prettier tern jsonlint csslint stylelint elm elm-test elm-oracle elm-format neovim

# Golang --------------------------------------------------------------------- #

brew install go-delve/delve/delve
go get github.com/alecthomas/gometalinter
gometalinter --install --update
go get github.com/motemen/ghq

# Rust ----------------------------------------------------------------------- #

cargo install racer exa

# Vim/Neovim ----------------------------------------------------------------- #

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Security ------------------------------------------------------------------- #
## TODO Get dnsmasq, privoxy, dnscrypt configs from other laptop and also https://github.com/drduh/macOS-Security-and-Privacy-Guide
## SSH config https://github.com/drduh/macOS-Security-and-Privacy-Guide#ssh

# macOS ---------------------------------------------------------------------- #

defaults write -g NSNavPanelExpandedStateForSaveMode -boolean true
defaults write -g PMPrintingExpandedStateForPrint -bool true

# https://github.com/drduh/macOS-Security-and-Privacy-Guide
## TODO Firmware password
sudo fdesetup enable # Filevault
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on # Firewall
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setloggingmode on # Firewall: logging
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setstealthmode on # Firewall: stealth mode
defaults write ~/Library/Preferences/com.apple.alf stealthenabled -bool true # Firewall: stealth mode
# sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.captive.control Active -bool false # Captive portal
defaults write com.apple.finder AppleShowAllFiles -bool true
chflags nohidden ~/Library
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
defaults write com.apple.TextEdit RichText -int 0
# Use current directory as default search scope in Finder
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
# Show Path bar in Finder
defaults write com.apple.finder ShowPathbar -bool true
# Show Status bar in Finder
defaults write com.apple.finder ShowStatusBar -bool true
# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
# Change screenshots location
defaults write com.apple.screencapture location ~/Desktop/screenshots

# Mail ----------------------------------------------------------------------- #

ln -s ~/Documents/dots/.mail .mail
ln -s ~/Documents/dots/.mbsyncrc .mbsyncrc

# Manual --------------------------------------------------------------------- #

echo "X-Fence"
echo "Gas Mask hosts files (Steve Black rmote) ? Or mackup"
echo "macOS security scripts --- https://github.com/drduh/macOS-Security-and-Privacy-Guide , https://github.com/kristovatlas/osx-config-check"
echo "TODO Safari extensions"
echo "^ covered above with security scipt --- Run through Lockdown app --- https://objective-see.com/products/lockdown.html"
echo "nvim +GoInstallBinaries"
echo "Sublime Text"
#  https://github.com/SublimeLinter/SublimeLinter-luacheck
echo "PopClip"
echo "Startup: Marathono, Hammerspoon, Tunnelblick, Hazel, Bartender, Clocker, 1Password, Oversight, PopClip, TripMode, Gas Mask, Adobe CC, NoSleep, Spectacle, Alfred, Endurance, DTerm, AutoVolume"
echo "TODO TextExpander"
echo "TODO AutoVolume"
echo "TODO Disable Monitor https://github.com/Eun/DisableMonitor"
echo "TODO Arq"
echo "TODO Time Machine"
echo "TODO Nix"
echo "Neomutt, mailsync"
echo "FF Nightly UI"
# https://spinscale.de/posts/2016-11-08-creating-a-productive-osx-environment-hammerspoon.html
# https://github.com/diimdeep/dotfiles/tree/master/osx/configure
