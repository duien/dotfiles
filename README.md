# Dotfiles


    homesick clone git@github.com:duien/dotfiles.git dotfiles
    homesick symlink dotfiles

Notes for initial setup:

Homesick never really ends up being the first thing installed, since it's necessary to set up homebrew and rbenv first. To get that install correct the first time, you'll need to set the rbenv install location:

    set -gx RBENV_ROOT /usr/local/var/rbenv

## Footnotes
 
 `mg` function from [Open Emacs magit from command line](https://trycatchchris.co.uk/post/view/Open-Emacs-magit-from-command-line)
