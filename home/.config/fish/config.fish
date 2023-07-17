# Path to your oh-my-fish.
# set fish_path $HOME/.oh-my-fish

# Path to your custom folder (default path is ~/.oh-my-fish/custom)
#set fish_custom $HOME/dotfiles/oh-my-fish

# Load oh-my-fish configuration.
# . $fish_path/oh-my-fish.fish

# Custom plugins and themes may be added to ~/.oh-my-fish/custom
# Plugins and themes can be found at https://github.com/oh-my-fish/
# Theme 'shish'
# Plugin 'theme'
# Plugin 'msg'

# Help the theme work
if contains (whoami) duien ehyland
	set -x default_user (whoami)
end

if [ -x '/opt/homebrew/bin/brew' ]
  /opt/homebrew/bin/brew shellenv fish | source
else if [ -x '/usr/local/bin/brew' ]
  /usr/local/bin/brew shellenv fish | source
end

# Load my aliases and environment
source ~/.config/fish/more.fish

source ("starship" init fish --print-full-init | psub)
