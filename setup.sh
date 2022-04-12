#!/usr/bin/env bash

set -euo pipefail


REPO='https://github.com/Procrat/dotfiles'
DEST="$HOME/repos/dotfiles"

VIM_PLUG_SCRIPT='https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
VIM_PLUG_DEST="$HOME/.config/nvim/autoload/plug.vim"

DEFAULT_SHELL='/bin/zsh'
ZPREZTO_REPO='https://github.com/sorin-ionescu/prezto.git'


ensure_repo_exists_and_has_latest_version() {
    repo="$1"
    dest="$2"
    if [[ -d "$dest" ]]; then
        mkdir -p "$(dirname "$dest")"
        (cd "$dest" && git pull origin)
    else
        git clone "$repo" "$dest"
    fi
}


echo 'Ensuring $HOME/.config exists...'
mkdir -p "$HOME/.config"

echo 'Ensuring repo exist locally...'
ensure_repo_exists_and_has_latest_version "$REPO" "$DEST"

echo 'Ensuring vim-plug is installed...'
curl -sSfLo "$VIM_PLUG_DEST" --create-dirs "$VIM_PLUG_SCRIPT"

echo 'Linking dotfiles...'
dotfiles=(
    aliases
    bashrc
    colors
    config/base16-shell
    config/bat
    'config/Code - OSS/User/settings.json'
    config/ctags
    config/environment.d
    config/gcalcli/gcalclirc
    config/git
    config/htop
    config/nvim/init.vim
    config/pip/requirements.in
    config/pylintrc
    config/ranger/rc.conf
    config/rofi
    config/skhd
    config/user-dirs.dirs
    config/yabai
    emacs.d/private
    ipython/profile_default/ipython_config.py
    profile
    shellrc
    spacemacs
    tern-config
    zpreztorc
    zprofile
    zshrc
)
for dotfile in "${dotfiles[@]}"; do
    mkdir -p "$(dirname "$HOME/.$dotfile")"
    ln -sfn "$DEST/$dotfile" "$HOME/.$dotfile"
done

echo 'Link bin folder...'
ln -sfn "$DEST/bin" "$HOME/bin"

echo "Ensuring $DEFAULT_SHELL is the default shell..."
if [[ -x "$DEFAULT_SHELL" ]]; then
    if [[ "$SHELL" != "$DEFAULT_SHELL" ]]; then
        chsh -s "$DEFAULT_SHELL"
    fi
    if [[ "$DEFAULT_SHELL" == '/bin/zsh'
        && ! -d '/usr/lib/prezto' && ! -d "${ZDOTDIR:-$HOME}/.zprezto" ]]; then
        git clone --recursive "$ZPREZTO_REPO" "${ZDOTDIR:-$HOME}/.zprezto"
    fi
else
    echo "Shell $DEFAULT_SHELL isn't installed on the system." >&2
fi

echo 'Updating vim-plug and NeoVim plugins...'
if [[ -n "$(which nvim 2>/dev/null)" ]]; then
    nvim +PlugUpgrade +qall
    nvim +PlugUpdate +qall
else
    echo 'No NeoVim installation was found.' >&2
fi
