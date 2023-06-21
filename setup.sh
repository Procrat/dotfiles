#!/usr/bin/env bash
#
# Install ALL the things.
#
# The entrypoint is `install_everything`. If you're not me, you probably want to
# pick and choose which things you want instead of installing a complete system
# that's completely customised to my liking.

set -euo pipefail


DOTFILES_DIR="$HOME/repos/dotfiles"



##################
### Entrypoint ###
##################


install_everything() {
    install_or_update_dotfiles

    setup_yay

    setup_system

    setup_bin_dir

    setup_shells

    setup_shell_tools

    setup_editors

    setup_development_tools

    setup_desktop_environment

    setup_background_services

    setup_applications
}



######################
### Main functions ###
######################


install_or_update_dotfiles() {
    ensure_packages_exist git

    ensure_repo_exists_and_has_latest_version \
        'https://github.com/Procrat/dotfiles' \
        "$DOTFILES_DIR"
}


setup_yay() {
    if command -v yay >/dev/null; then
        return
    fi

    note 'Installing yay'

    ensure_packages_exist base-devel git

    install_arch_package_from_git_repo 'https://aur.archlinux.org/yay-bin.git'

    link_dotfile 'config/yay'
}


setup_system() {
    context_note 'Ensuring system-level settings and daemons are set up'

    setup_keymap

    # Set up pacman & reflector
    link_etc_dotfile 'pacman.conf'
    ensure_packages_exist reflector
    link_etc_dotfile 'xdg/reflector/reflector.conf'
    # Periodically run reflector so we always have good mirrors
    ensure_systemd_unit_enabled reflector.timer
    # Periodically remove packages from the pacman cache
    ensure_systemd_unit_enabled paccache.timer

    # Run ufw firewall
    ensure_packages_exist ufw
    if [[ "$(sudo ufw status)" != "Status: active" ]]; then
        sudo ufw default deny
        sudo ufw enable
    fi

    # Automatically set the timezone based on our IP address
    link_etc_dotfile 'NetworkManager/dispatcher.d/no-wait.d/09-timezone'

    # Be a bit more lenient about locking out after failed login attempts
    link_etc_dotfile 'security/faillock.conf'

    # Slightly increase some limits to cope with modern system load
    link_etc_dotfile 'security/limits.conf'

    # Tune down swappiness so we still have an SSD at the end of the day
    link_etc_dotfile 'sysctl.d/99-swappiness.conf'

    # Make the power button suspend instead of shut down
    link_etc_dotfile 'systemd/logind.conf'

    # Send a notification when the battery level is critical
    link_etc_dotfile 'udev/rules.d/98-low-battery.rules'

    # Update systemd-boot loader on updates
    ensure_systemd_unit_enabled systemd-boot-update.service

    # Enable systemd-timesyncd daemon
    ensure_systemd_unit_enabled systemd-timesyncd.service

    # Periodically run TRIM on SSDs
    ensure_systemd_unit_enabled fstrim.timer

    # Other system utitilities
    ensure_packages_exist fwupd pkgstats
    ensure_aur_packages_exist downgrade

    # Lostfiles
    ensure_packages_exist lostfiles
    link_etc_dotfile 'lostfiles.conf'

    # Etckeeper
    ensure_packages_exist etckeeper
    link_etc_dotfile 'etckeeper/etckeeper.conf'
    if [[ ! -d /etc/.git ]]; then
        warn 'Etckeeper needs some initial set-up:'
        warn '  sudo etckeeper init'
        warn 'Then, connect it to a remote repo:'
        warn '  sudo etckeeper vcs remote add origin git@somerepo.git'
        warn "Make sure the root user's SSH config points to our SSH key:"
        warn "  echo 'Host <remote>\n    IdentityFile <your-private-key>' |" \
            'sudo tee /root/.ssh/config'
        warn 'And finally sync with the remote repo:'
        warn '  sudo etckeeper vcs fetch'
        warn '  sudo etckeeper vcs push --set-upstream origin master'
    fi
}

setup_keymap() {
    local layout='us'
    local model='pc105'
    local variant='altgr-intl'
    local options='ctrl:swapcaps'
    local current_keymap
    current_keymap=$(localectl status | awk '$1 == "X11" { print $3 }')
    local wanted_keymap
    wanted_keymap=$(printf '%s\n%s\n%s\n%s\n' \
        "$layout" "$model" "$variant" "$options")
    if [[ "$current_keymap" != "$wanted_keymap" ]]; then
        note 'Setting keymap'
        sudo localectl set-x11-keymap "$layout" "$model" "$variant" "$options"
    fi
}


setup_bin_dir() {
    context_note 'Ensuring all dependencies for bin directory are installed'

    # agenda
    setup_gcalcli
    # alarm
    ensure_packages_exist libcanberra libnotify mpv
    # bt-connect
    ensure_packages_exist bluez-utils
    setup_mydmenu
    # calc
    ensure_packages_exist bc
    # calendar-remind
    ensure_packages_exist networkmanager
    setup_gcalcli
    # detect_monitors
    ensure_packages_exist xorg-xrandr
    setup_rofi
    # dim-screen
    setup_light
    # https-server
    ensure_packages_exist python
    # lock-screen
    link_dotfile 'colors'
    ensure_aur_packages_exist i3lock-lixxia-git
    # memory-usage
    ensure_packages_exist valgrind
    # mydmenu
    setup_mydmenu
    # pyprofile
    ensure_packages_exist python
    # serve-wasm
    ensure_packages_exist python
    # shorten
    ensure_packages_exist curl python xclip
    # shrinkpdf
    ensure_packages_exist ghostscript
    # tad (Time Awareness Daemon)
    ensure_packages_exist sox
    # transfer
    ensure_packages_exist curl libnotify xclip zip
    # unsplash-background
    ensure_packages_exist curl jq
    setup_pass
    # update
    ensure_packages_exist pacman-contrib rustup
    setup_pass
    # view
    ensure_packages_exist feh
    # wallpaper
    ensure_packages_exist feh imagemagick xorg-xrandr
    # wifi-info
    ensure_packages_exist iw libnotify
    setup_alacritty
    # winwd
    ensure_packages_exist xorg-xprop
    # xdg-mime
    setup_handlr

    if [[ "$(readlink "$HOME/bin")" != "${DOTFILES_DIR}/bin" ]]; then
        note 'Linking bin directory'
        ln -sfn "${DOTFILES_DIR}/bin" "$HOME/bin"
    fi
}

setup_mydmenu() {
    # Note that this assumes the `bin` directory will get linked.
    ensure_packages_exist wmctrl
    ensure_aur_packages_exist dmenu2 yeganesh-bin
    setup_dzen  # FIXME: Only needed to know the bar height

    # dmenu settings & color scheme
    ensure_packages_exist ttf-ubuntu-font-family
    link_dotfile 'Xresources'
}


# Shell

setup_shells() {
    context_note 'Ensuring shells are set up'
    setup_bash
    setup_zsh
    set_default_shell '/bin/zsh'
}

setup_bash() {
    # Zsh first sources `profile` and then `bashrc`

    ensure_packages_exist bash bash-completion

    link_dotfile 'profile'

    setup_shellrc

    # Completion and history search with FZF
    ensure_packages_exist fzf

    link_dotfile 'bashrc'
}

setup_zsh() {
    # Zsh first sources `zprofile` (symlink to `profile`) and then `zshrc`

    ensure_packages_exist zsh zsh-completions

    link_dotfile 'zprofile'
    link_dotfile 'profile'

    setup_shellrc

    # Set up prezto
    ensure_aur_packages_exist prezto-git
    # Don't initialise default prezto stuff; we'd end up overriding everything
    # anyway. (I don't think the prezto package should be placing a
    # /etc/zsh/zshrc file in the first place.)
    link_etc_dotfile 'zsh/zshrc'
    link_dotfile 'zpreztorc'

    # Prompt
    ensure_packages_exist git terraform

    # Completion and history search with FZF
    ensure_packages_exist fzf

    # Inline aliases
    ensure_packages_exist awk curl ripgrep

    link_dotfile 'zshrc'
}

setup_shellrc() {
    # ~/.shellrc are a collection of shell-independent settings
    setup_shell_aliases
    ensure_aur_packages_exist fnm-bin
    link_dotfile 'shellrc'
}

setup_shell_aliases() {
    ensure_packages_exist \
        exa \
        fd \
        fzf \
        git \
        ipython \
        neovim \
        python \
        ranger \
        ripgrep \
        terraform
    setup_handlr
    setup_pass
    link_dotfile 'aliases'
}

set_default_shell() {
    local shell="$1"
    local current_shell
    current_shell=$(getent passwd "$USER" | cut -d: -f7)
    if [[ "$current_shell" != "$shell" ]]; then
        note "Setting $shell as the default shell"
        chsh -s "$shell"
    fi
}

setup_shell_tools() {
    context_note 'Ensuring shell tools are set up'

    # bat
    setup_bat

    # gcalcli
    setup_gcalcli

    # Git
    ensure_packages_exist diff-so-fancy git git-lfs gnupg neovim
    ensure_aur_packages_exist git-extras
    link_dotfile 'config/git'

    # IPython
    ensure_packages_exist ipython
    link_dotfile 'config/ipython/profile_default/ipython_config.py'

    # ranger
    ensure_packages_exist ranger
    link_dotfile 'config/ranger/rc.conf'

    # System monitoring tools
    ensure_packages_exist \
        dust \
        glances \
        iftop \
        iotop \
        lsof \
        ncdu \
        nethogs
    ensure_aur_packages_exist \
        dockviz \
        fatrace
    # htop
    ensure_packages_exist htop
    link_dotfile 'config/htop'
    # top (installed by default under procps-ng)
    link_dotfile 'config/procps/toprc'

    # Network debugging tools
    ensure_packages_exist \
        bind \
        mtr \
        nmap \
        whois

    # Data wrangling tools
    ensure_packages_exist \
        dos2unix \
        jq \
        moreutils \
        perl-image-exiftool \
        pv \
        ripgrep \
        unrar \
        unzip \
        xsv \
        zip

    # Other useful tools
    ensure_packages_exist \
        bc \
        fd \
        hyperfine \
        plocate \
        strace \
        words
    ensure_aur_packages_exist \
        faketty
    ensure_systemd_unit_enabled plocate-updatedb.timer
}


# Editors & development environment

setup_editors() {
    setup_neovim
    # setup_spacemacs
    # setup_vscode
}

setup_neovim() {
    context_note 'Ensuring Neovim is set up'

    ensure_packages_exist neovim

    # DEPENDENCIES
    # ------------
    #
    # xclip or XSel is needed for synchronisation with X11 clipboard
    ensure_packages_exist xclip
    # We use ripgrep as `grepprg`
    ensure_packages_exist ripgrep
    # junegunn/fzf.vim requires fzf and optionally rg for :Rg and bat for syntax
    # highlighting in previews
    setup_bat
    ensure_packages_exist fzf ripgrep
    # nvim-treesitter/nvim-treesitter optionally requires the tree-sitter
    # CLI for automatically installing parsers when entering a buffer
    ensure_packages_exist tree-sitter
    # shime/vim-livedown
    ensure_aur_packages_exist nodejs-livedown
    # tpope/vim-fugitive
    ensure_packages_exist git
    # lervag/vimtex requires a LaTeX compilation backend
    ensure_packages_exist tectonic
    # nvim-tree/nvim-web-devicons requires a Nerd Font
    setup_fonts
    # simrat39/rust-tools.nvim
    setup_rust
    # jose-elias-alvarez/null-ls.nvim
    ensure_packages_exist \
        eslint_d \
        flake8 \
        jq \
        luacheck \
        prettier \
        python-isort \
        stylelint \
        terraform \
        vint
    ensure_aur_packages_exist shellcheck-bin stylish-haskell-bin
    # akinsho/bufferline.nvim requires a Nerd Font
    setup_fonts
    # onsails/lspkind.nvim requires a Nerd Font
    setup_fonts
    # tamago324/cmp-zsh
    ensure_packages_exist zsh
    # LSP servers
    ensure_packages_exist \
        lua-language-server \
        pyright \
        typescript-language-server
    setup_vue

    # Install vim-plug and all plugins
    local vim_plug_path="$HOME/.local/share/nvim/site/autoload/plug.vim"
    if [[ ! -f "$vim_plug_path" ]]; then
        note 'Installing vim-plug'
        ensure_packages_exist curl
        curl -sSfLo "$vim_plug_path" --create-dirs \
            'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

        link_dotfile 'config/nvim/init.vim'

        note 'Installing Neovim plugins'
        nvim +PlugInstall +qall
    fi
}

setup_spacemacs() {
    context_note 'Ensuring Spacemacs is set up'

    # Font
    ensure_aur_packages_exist otf-monaco-powerline-font-git

    # Optional dependencies
    ensure_packages_exist \
        docker \
        editorconfig-core-c \
        git \
        python-lsp-server \
        ripgrep \
        typescript-language-server
    ensure_aur_packages_exist hlint-bin shellcheck-bin stylish-haskell-bin
    setup_rust
    setup_vue

    link_dotfile 'emacs.d/private'
    link_dotfile 'spacemacs'
    ensure_packages_exist emacs
}

setup_vscode() {
    context_note 'Ensuring VS Code is set up'
    ensure_packages_exist code
    link_dotfile 'config/Code - OSS/User/settings.json'
}

setup_development_tools() {
    context_note 'Ensuring development tools are installed'

    # Python
    ensure_packages_exist \
        ipython \
        python-ipdb \
        python-pipenv \
        python-poetry
    ensure_aur_packages_exist pipdeptree

    # Rust
    ensure_packages_exist \
        cargo-bloat \
        cargo-edit \
        cargo-flamegraph \
        cargo-outdated \
        cargo-watch
    ensure_aur_packages_exist cargo-cache

    # Web development
    ensure_packages_exist \
        dart-sass \
        yarn
    ensure_aur_packages_exist \
        nodejs-npm-upgrade \
        nodejs-surge \
        fnm-bin

    # Cloud management
    ensure_packages_exist terraform
    ensure_aur_packages_exist ngrok
    setup_aws
}

setup_aws() {
    # We use aws-vault with pass as its backend for credential management
    ensure_packages_exist aws-cli aws-vault
    # Requires a password to exist in `pass` for the relevant AWS profile
    setup_pass
    link_dotfile 'aws'
}

setup_rust() {
    ensure_packages_exist rustup
    rustup component add clippy rust-analyzer rustfmt 2>&1 | \
        grep -v ' is up to date' || true
}

setup_vue() {
    # For Vue 2
    ensure_aur_packages_exist nodejs-vls
    # For Vue 3
    ensure_aur_packages_exist volar-server-bin
}


# Desktop environment

setup_desktop_environment() {
    context_note 'Ensuring desktop environment is set up'
    ensure_packages_exist xorg-server
    setup_xinit
    setup_window_manager
    setup_bar
    setup_launchers
    setup_notifications
    setup_external_mouse
    setup_user_dirs
    context_note 'Ensuring fonts are set up'
    setup_fonts
    setup_gtk_and_qt
}

setup_external_mouse() {
    # Make external mouse move a bit faster
    link_etc_dotfile 'X11/xorg.conf.d/99-libinput-custom-config.conf'
}

setup_xinit() {
    context_note 'Ensuring all packages for xinit are installed'
    ensure_packages_exist \
        feh \
        picom \
        unclutter \
        xorg-xdpyinfo \
        xorg-xinit \
        xorg-xset \
        xss-lock
    ensure_aur_packages_exist i3lock-lixxia-git touchegg
    setup_keychain
    setup_light
    setup_redshift
    setup_touchegg
    link_dotfile 'xinitrc'
    link_dotfile 'xprofile'
}

setup_window_manager() {
    context_note 'Ensuring XMonad is set up'
    ensure_packages_exist libx11 libxft libxinerama libxrandr libxss xorg-xmessage
    ensure_aur_packages_exist stack-static
    link_dotfile 'config/xmonad'
    if ! command -v xmonad >/dev/null; then
        (
            cd "$HOME/.config/xmonad"
            stack install
            # Recompilation installs the executable into ~/.cache which we link
            # to from bin/xmonad
            "$HOME/.local/bin/xmonad-x86_64-linux" --recompile
            rm "$HOME/.local/bin/xmonad-x86_64-linux"
        )
    fi

    context_note 'Ensuring media keys work in XMonad'
    # For brightness control
    ensure_packages_exist wmctrl
    setup_dzen
    setup_light
    # For music control
    ensure_packages_exist playerctl
    # For volume control
    ensure_packages_exist libpulse wmctrl
    setup_dzen
}

setup_bar() {
    context_note 'Ensuring xmobar is set up'

    if ! command -v xmobar >/dev/null; then
        install_arch_package_from_git_repo \
            'https://github.com/Procrat/xmobar-arch-package-stack'
    fi

    # Fonts
    ensure_packages_exist ttf-dejavu ttf-ubuntu-font-family

    # Icons
    link_dotfile 'config/icons'

    # Dependencies for the wifi info notification are already installed when
    # setting up the bin directory

    # For openinging calendar when clicking on date
    setup_handlr

    link_dotfile 'config/xmobar'
}

setup_launchers() {
    context_note 'Ensuring all applications for launchers are installed'
    ensure_packages_exist \
        j4-dmenu-desktop \
        pavucontrol \
        ranger \
        rofi-calc \
        rofi-pass \
        rofimoji \
        wmctrl \
        xorg-xprop \
        xterm \
        zsh
    ensure_aur_packages_exist brave-bin
    setup_alacritty
    setup_handlr
    setup_mydmenu
    setup_pass
    setup_rofi
    link_dotfile 'Xresources'  # For xterm settings
}

setup_notifications() {
    context_note 'Ensuring notification system is set up'
    ensure_packages_exist adwaita-icon-theme libnotify ttf-ubuntu-font-family
    setup_mydmenu
    setup_handlr
    ensure_packages_exist dunst
    link_dotfile 'config/dunst'
}

setup_user_dirs() {
    context_note 'Ensuring user dirs are set up'
    ensure_packages_exist xdg-user-dirs
    link_dotfile 'config/user-dirs.dirs'
}

setup_fonts() {
    # Use noto as default font since it covers a lot
    ensure_packages_exist noto-fonts noto-fonts-cjk noto-fonts-emoji

    # Metric-compatible fonts for Arial, Times New Roman, and Courier New
    ensure_packages_exist ttf-liberation

    # Use MonacoB2 with dev icons as default monospace font
    ensure_aur_packages_exist otf-nerd-fonts-monacob-mono ttf-monaco

    # A collection of icon fonts for fancy dev icons in the terminal
    ensure_aur_packages_exist ttf-all-the-icons

    # Sets default monospace font and replaces MS fonts with free ones
    link_dotfile 'config/fontconfig'
}

setup_gtk_and_qt() {
    context_note 'Ensuring GTK & Qt set up'

    ensure_packages_exist arc-gtk-theme
    # Adwaita icon theme is installed as a dependency of both GTK3 and GTK4,
    # so we don't have to install it explicitly.
    setup_fonts

    link_dotfile 'gtkrc-2.0'
    link_dotfile 'config/gtk-3.0/settings.ini'
    # Note that the GTK theme is actually taken by the one set in .xinitrc
    link_dotfile 'config/gtk-4.0/settings.ini'

    # The Qt theme is set in .xinitrc
    ensure_packages_exist adwaita-qt5 adwaita-qt6
}


# Background services

setup_background_services() {
    context_note 'Ensuring user-level daemons are set up'

    # For background.service, reminder.service and tad.service
    setup_bin_dir

    # For backup.service
    ensure_packages_exist restic
    setup_pass
    setup_aws

    # For backup-passwords.service
    setup_pass

    # For syncthing.service
    ensure_packages_exist syncthing

    # For watchdog.service
    ensure_packages_exist libnotify

    link_dotfile 'config/environment.d'
    link_dotfile 'config/systemd'

    systemctl --user daemon-reload
}


# (Graphical) applications

setup_applications() {
    context_note 'Ensuring (graphical) applications are installed'

    ensure_packages_exist \
        bully \
        calibre \
        chromium \
        docker \
        electrum \
        eog \
        evince \
        firefox \
        gimp \
        gnome-disk-utility \
        imv \
        libreoffice-still \
        nautilus \
        obsidian \
        pavucontrol \
        puzzles \
        rofi-calc \
        rofi-pass \
        rofimoji \
        signal-desktop \
        transmission-gtk \
        wine-gecko \
        wine-mono \
        xarchiver \
        xorg-xkill
    ensure_aur_packages_exist \
        brave-bin \
        ferdium-bin \
        mycrypto-bin \
        nsxiv
    setup_alacritty
    setup_mpv
    setup_pass
    setup_rofi
    setup_steam

    link_dotfile 'config/mimeapps.list'
    link_dotfile 'local/share/applications'
    # Override the other mimeapps.list that applications sometimes write to
    link_dotfile "$HOME/.local/share/applications/mimeapps.list" \
        "$HOME/.config/mimeapps.list"
}


# Set-up for individual packages

setup_alacritty() {
    # Alacritty is a terminal emulator
    ensure_packages_exist alacritty
    link_dotfile 'config/alacritty'
}

setup_bat() {
    # bat is like cat but with syntax highlighting and auto-paging
    ensure_packages_exist bat
    link_dotfile 'config/bat'
}

setup_dzen() {
    # Dzen is a lot of things, but we use it as a notification bar specifically
    # when adjusting the volume or brightness.
    ensure_packages_exist dzen2 ttf-ubuntu-font-family
    link_dotfile 'config/icons'
    link_dotfile 'config/dzen'
    link_dotfile 'Xresources'
}

setup_gcalcli() {
    ensure_aur_packages_exist gcalcli
    link_dotfile 'config/gcalcli/gcalclirc'
    if [[ ! -f "$HOME/.config/gcalcli/oauth" ]]; then
        warn 'gcalcli needs some initial authentication set-up. Run:'
        # shellcheck disable=SC2016
        warn '  gcalcli --config-folder ~/.config/gcalcli' \
            '--client-id=$OATH_CLIENT_ID' \
            '--client-secret=$OAUTH_CLIENT_SECRET' \
            'list'
    fi
}

setup_handlr() {
    # Handlr is much more sane than FreeDesktop's xdg-open
    ensure_packages_exist handlr
    ensure_aur_packages_exist xdg-utils-handlr
}

setup_keychain() {
    # Keychain is an SSH agent and GPG agent

    # For SSH agent
    ensure_aur_packages_exist gnome-ssh-askpass3
    setup_ssh

    # For GPG agent
    ensure_packages_exist gcr
    link_dotfile 'gnupg/gpg-agent.conf'

    ensure_packages_exist keychain
}

setup_light() {
    # Light is a brightness controller
    ensure_packages_exist light
    ensure_user_in_group video
}

setup_mpv() {
    # mpv is a media player
    ensure_packages_exist mpv
    link_dotfile 'config/mpv'
}

setup_pass() {
    # Pass is a password manager
    ensure_packages_exist pass
    setup_ssh
    if [[ ! -d "$HOME/.password-store" ]]; then
        warn "Password store isn't created yet."
        warn "Create a new one with:"
        warn "    pass init <GPG-key-id>"
        warn "Or connect to an existing remote repo with:"
        warn "    pass git init"
        warn "    pass git remote add origin <remote-repo>"
    fi
}

setup_redshift() {
    # Redshift turns the screen slightly red at night
    ensure_aur_packages_exist redshift-minimal
    link_dotfile 'config/redshift'
}

setup_rofi() {
    # Rofi is a menu, like dmenu
    ensure_packages_exist rofi ttf-ubuntu-font-family
    link_dotfile 'config/rofi'
}

setup_ssh() {
    ensure_packages_exist openssh

    if ! ls "$HOME/.ssh/"*'.pub' &>/dev/null; then
        note 'Generating new SSH key pair'
        ssh-keygen -t ed25519
    fi

    link_dotfile 'ssh/config'
}

setup_steam() {
    if ! grep '^\[multilib\]$' /etc/pacman.conf &>/dev/null; then
        note 'To install Steam, first enable the multilib repository in /etc/pacman.conf'
        return 1
    fi

    # Out of scope: Vulkan and OpenGL drivers for your hardware
    ensure_packages_exist steam ttf-liberation
}

setup_touchegg() {
    # Touchegg is a tool to attach actions to trackpad gestures.
    # Touché is its front-end.
    ensure_aur_packages_exist touche touchegg
    ensure_systemd_unit_enabled touchegg.service
    link_dotfile 'config/touchegg/touchegg.conf'
}


#########################
### Utility functions ###
#########################


link_dotfile() {
    if [[ $# -eq 1 ]]; then
        local dotfile="$1"
        local link="$HOME/.$dotfile"
        local target="${DOTFILES_DIR}/${dotfile}"
        local note="Linking dotfile $dotfile"
    elif [[ $# -eq 2 ]]; then
        local link="$1"
        local target="$2"
        local note="Linking dotfile $link to $target"
    fi
    if [[ "$(readlink "$link")" != "$target" ]]; then
        note "$note"
        mkdir -p "$(dirname "$link")"
        ln -sfnT "$target" "$link"
    fi
}

link_etc_dotfile() {
    local dotfile="$1"
    local link="/etc/${dotfile}"
    local target="${DOTFILES_DIR}/etc/${dotfile}"
    if [[ "$(readlink "$link")" != "$target" ]]; then
        note "Linking dotfile /etc/$dotfile"
        sudo mkdir -p "$(dirname "$link")"
        sudo ln -sfnT "$target" "$link"
    fi
}

ensure_packages_exist() {
    _ensure_packages_exist_with_package_manager "sudo pacman" "$@"
}

ensure_aur_packages_exist() {
    _ensure_packages_exist_with_package_manager "yay" "$@"
}

_ensure_packages_exist_with_package_manager() {
    local package_manager="$1"
    shift
    for package in "$@"; do
        if ! $package_manager -Q "$package" >/dev/null 2>&1; then
            note "Installing $package with $package_manager"
            $package_manager -Sy --needed "$package"
        fi
    done
}

ensure_user_in_group() {
    local group="$1"
    if ! groups | grep -qw "$group"; then
        note "Adding $USER to group $group"
        sudo gpasswd -a "$USER" video
    fi
}

ensure_systemd_unit_enabled() {
    local unit="$1"
    local enabled
    enabled=$(systemctl is-enabled "$unit")
    local active
    active=$(systemctl is-active "$unit")
    if [[ "$enabled" != enabled || "$active" != active ]]; then
        note "Enabling and starting systemd unit $unit"
        sudo systemctl enable --now "$unit"
    fi
}

note() {
    tput setaf 3  # Yellow
    tput bold
    echo
    echo "[[[ $*... ]]]"
    tput sgr0  # Reset color and boldness
}

context_note() {
    tput setaf 8  # Background highlight
    echo
    echo "[[[ $*... ]]]"
    tput sgr0  # Reset color and boldness
}

warn() {
    tput setaf 1  # Yellow
    tput bold
    echo
    echo '!!!' "$*"
    tput sgr0  # Reset color and boldness
}

ensure_repo_exists_and_has_latest_version() {
    local repo="$1"
    local dest="$2"
    if [[ ! -d "$dest" ]]; then
        note "Installing dotfiles repo at $dest"
        git clone --recursive "$repo" "$dest"
    else
        note 'Pulling down latest version of dotfiles repo'
        (
            cd "$dest"
            git pull origin
            git submodule update --init --recursive
        )
    fi
}

install_arch_package_from_git_repo() {
    local url="$1"

    ensure_packages_exist git
    local dir
    dir="$(mktemp -d)"
    git clone "$url" "$dir"
    (
        cd "$dir"
        makepkg -si
    )
    rm -rf "$dir"
}



##################
### Entrypoint ###
##################


install_everything
