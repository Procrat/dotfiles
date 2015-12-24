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

echo 'Ensuring $HOME/.config exists...'
mkdir -p "$HOME/.config"

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
    config/user-dirs.dirs
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
    mkdir -p "$(dirname $HOME/.$dotfile)"
    ln -sfn "$DEST/$dotfile" "$HOME/.$dotfile"
done

echo 'Link NeoVim config files to Vim config files...'
ln -sfn "$HOME/.vim" "$HOME/.config/nvim"
ln -sfn "$HOME/.vimrc" "$HOME/.config/nvim/init.vim"

echo 'Setting crontab...'
if which crontab 2>/dev/null >&2; then
    crontab "$DEST/crontab"
else
    echo 'No cron handler found.' >&2
fi

# Really, Freedesktop? There is no hope left if you don't even follow your own
# standards.
if [[ -e "$HOME/.local/share/applications/mimeapps.list" ]]; then
    ln -sfn "$HOME/.config/mimeapps.list" \
        "$HOME/.local/share/applications/mimeapps.list"
fi

echo "Ensuring $DEFAULT_SHELL is the default shell..."
shell=$(getent passwd $USER | cut -d: -f7)
if [[ x"$shell" != x"$DEFAULT_SHELL" ]]; then
    if [[ -e "$DEFAULT_SHELL" ]]; then
        chsh -s "$DEFAULT_SHELL"
    else
        echo "Shell $DEFAULT_SHELL does not exist." >&2
    fi
fi

echo 'Updating (Neo)Vim plugins...'
my_vim="$(which nvim 2>/dev/null || which vim 2>/dev/null)"
if [[ -n "$my_vim" ]]; then
    "$my_vim" +PluginInstall! +qall
else
    echo 'No Vim installation was found.' >&2
fi

reset
