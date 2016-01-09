#!/usr/bin/env bash

set -euo pipefail


REPO='https://github.com/Procrat/dotfiles'
DEST="$HOME/repos/dotfiles"

TPM_REPO='https://github.com/tmux-plugins/tpm'
TPM_DEST="$HOME/.tmux/plugins/tpm"

VIM_PLUG_SCRIPT='https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
VIM_PLUG_DEST="$HOME/.config/nvim/autoload/plug.vim"

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


echo 'Ensuring $HOME/.config exists...'
mkdir -p "$HOME/.config"

echo 'Ensuring repo exist locally...'
ensure_repo_exists_and_has_latest_version "$REPO" "$DEST"

echo 'Ensuring Tmux Plugin Manager is installed...'
ensure_repo_exists_and_has_latest_version "$TPM_REPO" "$TPM_DEST"

echo 'Ensuring vim-plug is installed...'
curl -fLo "$VIM_PLUG_DEST" --create-dirs "$VIM_PLUG_SCRIPT"

echo 'Linking dotfiles...'
dotfiles=(
    aliases
    bash_profile
    bashrc
    colors
    config/base16-shell
    config/herbstluftwm
    config/htop
    config/icons
    config/mimeapps.list
    config/nvim/init.vim
    config/nvim/ycm_extra_conf.py
    config/systemd
    config/user-dirs.dirs
    gitconfig
    gitignore_global
    ipython/profile_default/ipython_config.py
    local/share/applications
    shellrc
    ssh/config
    tmux.conf
    xinitrc
    xprofile
    Xresources
    zpreztorc
    zprofile
    zshrc
)
for dotfile in ${dotfiles[@]}; do
    mkdir -p "$(dirname $HOME/.$dotfile)"
    ln -sfn "$DEST/$dotfile" "$HOME/.$dotfile"
done

echo 'Link Vim config files to NeoVim config files (just to be safe)...'
ln -sfn "$HOME/.config/nvim" "$HOME/.vim"
ln -sfn "$HOME/.config/nvim/init.vim" "$HOME/.vimrc"

echo 'Link bin folder...'
ln -sfn "$DEST/bin" "$HOME/bin"

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

echo 'Updating vim-plug and NeoVim plugins...'
if [[ -n "$(which nvim 2>/dev/null)" ]]; then
    nvim +PlugUpgrade +qall
    nvim +PlugUpdate +qall
else
    echo 'No NeoVim installation was found.' >&2
fi
