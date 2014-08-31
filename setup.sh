#!/usr/bin/env bash

REPO=http://github.com/Procrat/dotfiles
DEST=$HOME/Documenten/dotfiles

VUNDLE_REPO=https://github.com/gmarik/Vundle.vim.git
VUNDLE_PATH=$HOME/.vim/bundle/Vundle.vim


echo 'Ensuring repo exist locally...'
if [[ -d $DEST ]]; then
    mkdir -p $(dirname $DEST)
    (cd $DEST && git pull origin master)
else
    git clone $REPO $DEST
fi

echo 'Ensuring Vundle is installed...'
if [[ -d $VUNDLE_PATH ]]; then
    mkdir -p $(dirname $VUNDLE_PATH)
    (cd $VUNDLE_PATH && git pull origin master)
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
    Xresources
    ssh/config
)
for dotfile in $dotfiles; do
    ln -sfn $DEST/$dotfile $HOME/.$dotfile
done

echo 'Updating Bundles...'
vim +PluginInstall! +qall

reset
