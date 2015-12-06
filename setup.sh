#!/usr/bin/env bash

set -euo pipefail


REPO='https://github.com/Procrat/dotfiles'
DEST="$HOME/repos/dotfiles"

TPM_REPO='https://github.com/tmux-plugins/tpm'
TPM_DEST="$HOME/.tmux/plugins/tpm"

VUNDLE_REPO='https://github.com/gmarik/Vundle.vim'
VUNDLE_DEST="$HOME/.vim/bundle/Vundle.vim"

DEFAULT_SHELL='/bin/zsh'


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
    aliases
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
    shellrc
    ssh/config
    statnotrc
    tmux.conf
    vimrc
    vim/ycm_extra_conf.py
    xinitrc
    xprofile
    Xresources
    zpreztorc
    zshrc
)
for dotfile in ${dotfiles[@]}; do
    ln -sfn "$DEST/$dotfile" "$HOME/.$dotfile"
done

echo 'Setting crontab...'
cp crontab "/var/spool/cron/$USER"

# Really, Freedesktop? There is no hope left if you don't even follow your own
# standards.
ln -sfn "$HOME/.config/mimeapps.list" \
    "$HOME/.local/share/applications/mimeapps.list"

echo "Ensuring $DEFAULT_SHELL is the default shell..."
shell=$(getent passwd $USER | cut -d: -f7)
if [[ x"$shell" != x"$DEFAULT_SHELL" ]]; then
    chsh -s "$DEFAULT_SHELL"
fi

echo 'Updating Bundles...'
vim +PluginInstall! +qall

reset
