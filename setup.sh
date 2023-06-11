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
    aws
    bashrc
    colors
    config/alacritty
    config/bat
    'config/Code - OSS/User/settings.json'
    config/dunst
    config/dzen
    config/environment.d
    config/fontconfig
    config/gcalcli/gcalclirc
    config/git
    config/gtk-3.0/settings.ini
    config/htop
    config/icons
    config/ipython/profile_default/ipython_config.py
    config/mimeapps.list
    config/nvim/init.vim
    config/ranger/rc.conf
    config/redshift
    config/rofi
    config/systemd
    config/procps/toprc
    config/touchegg/touchegg.conf
    config/user-dirs.dirs
    config/xmobar
    config/xmonad
    config/yay
    emacs.d/private
    gtkrc-2.0
    local/share/applications
    profile
    shellrc
    spacemacs
    ssh/config
    xinitrc
    xprofile
    Xresources
    zpreztorc
    zprofile
    zshrc
)
for dotfile in "${dotfiles[@]}"; do
    mkdir -p "$(dirname "$HOME/.$dotfile")"
    ln -sfnT "$DEST/$dotfile" "$HOME/.$dotfile"
done

echo 'Link bin folder...'
ln -sfn "$DEST/bin" "$HOME/bin"

# Really, Freedesktop? There is no hope left if you don't even follow your own
# standards.
if [[ -e "$HOME/.local/share/applications/mimeapps.list" ]]; then
    ln -sfn "$HOME/.config/mimeapps.list" \
        "$HOME/.local/share/applications/mimeapps.list"
fi

echo "Ensuring $DEFAULT_SHELL is the default shell..."
if [[ -x "$DEFAULT_SHELL" ]]; then
    shell=$(getent passwd "$USER" | cut -d: -f7)
    if [[ "$shell" != "$DEFAULT_SHELL" ]]; then
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
