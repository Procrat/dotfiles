#!/usr/bin/env bash

set -euo pipefail


REPO='https://github.com/Procrat/dotfiles'
DEST="$HOME/repos/dotfiles"

TPM_REPO='https://github.com/tmux-plugins/tpm'
TPM_DEST="$HOME/.tmux/plugins/tpm"

VUNDLE_REPO='https://github.com/gmarik/Vundle.vim'
VUNDLE_DEST="$HOME/.vim/bundle/Vundle.vim"


ensure_repo_exists_and_has_latest_version() {
    repo="$1"
    dest="$2"
    if [[ -d "$dest" ]]; then
        mkdir -p $(dirname "$dest")
        (cd "$dest" && git pull origin master)
    else
        git clone "$repo" "$dest"
    fi
}


echo 'Ensuring repo exist locally...'
ensure_repo_exists_and_has_latest_version "$REPO" "$DEST"

echo 'Ensuring Tmux Plugin Manager is installed...'
ensure_repo_exists_and_has_latest_version "$TPM_REPO" "$TPM_DEST"

echo 'Ensuring Vundle is installed...'
ensure_repo_exists_and_has_latest_version "$VUNDLE_REPO" "$VUNDLE_DEST"

echo 'Linking dotfiles...'
dotfiles=(
    bash_aliases
    bashrc
    colors
    config/base16-shell
    config/herbstluftwm
    config/htop
    config/icons
    config/mimeapps.list
    config/systemd
    devilspie
    gitconfig
    gitignore_global
    ipython/profile_default/ipython_config.py
    offlineimaprc
    ssh/config
    statnotrc
    tmux.conf
    vimrc
    vim/ycm_extra_conf.py
    xinitrc
    xprofile
    Xresources
)
for dotfile in ${dotfiles[@]}; do
    ln -sfn "$DEST/$dotfile" "$HOME/.$dotfile"
done

echo 'Updating Bundles...'
vim +PluginInstall! +qall

reset
