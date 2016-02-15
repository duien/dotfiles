# node version manager
set -x NVM_DIR "$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ]; and bass source "$NVM_DIR/nvm.sh"

function nvm
  bass source $NVM_DIR/nvm.sh ';' nvm $argv
end
