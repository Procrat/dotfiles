#!/usr/bin/env bash

REPO=http://github.com/Procrat/dotfiles
DEST=~/Documenten/dotfiles

VUNDLE_REPO=https://github.com/gmarik/vundle.git
VUNDLE_PATH=$HOME/.vim/bundle/vundle


echo 'Ensuring repo exist locally...'
if [[ -d $DEST ]]; then
    (cd $DEST && git pull origin master)
else
    git clone $REPO $DEST
fi

echo 'Ensuring Vundle is installed...'
if [[ -d $VUNDLE_PATH ]]; then
    (cd $DEST && git pull origin master)
else
    git clone $VUNDLE_REPO $VUNDLE_PATH
fi

echo 'Linking dotfiles...'
dotfiles=(
    bash_aliases
    bashrc
    gitconfig
    gitignore_global
    tmux.conf
    vimrc
)
for dotfile in $dotfiles; do
    ln -sfn $DEST/$dotfile $HOME/.$dotfile
done

echo 'Updating Bundles...'
vim +BundleInstall! +qall

reset
