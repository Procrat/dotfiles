Procrat's dotfiles
==================

Some parts are dependent on each other, so be careful when blindly copying
certain parts.

Dependency tree
---------------

- Bash
  - base16-shell (git submodule)
  - See _Shell aliases_
- bat
- Custom dmenu (`bin/mydmenu`)
  - dmenu2
  - yeganesh
- Dunst
  - Adwaita icon theme
  - Font: Ubuntu Mono
  - For actions / context menu:
    - Custom dmenu
    - libnotify
    - xdg-utils/handlr
- gcalcli
- Git
  - Neovim (as merge tool)
- GTK 2 & GTK 3
  - Arc-Dark theme
  - Adwaita icon theme
  - Font: Cantarell
- htop
- IPython
- Neovim 0.5+
  - Neovim and tmux interact through the M-[hjkl] bindings (see
    `config/nvim/init.vim` and `tmux.conf`)
  - vim-plug
  - For all plugins, see `config/nvim/init.vim`
  - fzf
  - Git
  - isort
  - pyls
  - Python 3
  - racer
  - ripgrep
  - stylish-haskell
  - Universal Ctags
  - VLS (Vue Language Server)
  - xclip or XSel for synchronisation with X11 clipboard
  - A bunch of optional linting tools (ChkTeX, ESLint, HLint, Flake8, Pylint,
    RLS, ShellCheck, stylelint, TFLint, Vint, ...)
- Pylint
  - For dealing with virtual environments: pylint-venv
- ranger
- Rofi
- Shell aliases:
  - Emacs
  - exa
  - Git
  - IPython
  - isort
  - Mosh
  - Neovim
  - pass
  - Pygments
  - ranger
  - ripgrep
  - systemd
  - Terraform
  - Translate Shell
  - xdg-utils/handlr
- Spacemacs (all optional and enabled with layers)
  - Monaco for Powerline font
  - For faster searching: ripgrep
  - For docker layer: Docker
  - For git layer: Git
  - For Haskell layer: hasktags, HLint, Hoogle, stylish-haskell, HIE/HLS
  - For JavaScript: Tern
  - For Python: pyls
  - For Rust: RLS
  - For shell: ShellCheck
  - For TypeScript: TypeScript language server
  - For Vue: VLS (Vue Language Server)
- systemd
- Tern
- tmux
  - Neovim and tmux interact through the M-[hjkl] bindings (see
    `config/nvim/init.vim` and `tmux.conf`)
  - TPM (Tmux Plugin Manager)
  - xclip
- Universal Ctags
- X11/xinitrc
  - feh
  - gnome-keyring-daemon
  - picom
  - Redshift
  - unclutter
  - URxvt
  - Xautolock
  - XMonad
  - xrdb
  - xset
- xmobar
  - Fonts: Ubuntu & DejaVu Sans
  - alsa-utils
  - libnotify
  - Icons (defined in `config/icons/`)
  - xdg-utils/handlr (for xdg-open)
- XMonad
  - [Procrat/XMonad.Actions.Contexts](https://github.com/Procrat/xmonad-contexts) (git submodule)
    - Custom dmenu
  - [Procrat/XMonad.Layout.Pseudotiling](https://github.com/Procrat/xmonad-pseudotiling) (git submodule)
  - [Procrat/XMonad.Layout.SingleSpacing](https://github.com/Procrat/xmonad-singlespacing) (git submodule)
  - Launchers
    - handlr
    - j4-dmenu-desktop
      - Custom dmenu
      - URxvt
    - pavucontrol
    - ranger
    - rofi-calc
    - rofimoji
    - rofi-pass
    - URxvt
    - xterm as fallback terminal
  - Media keys
    - For brightness control: light, dzen2-xft-xpm
    - For music control: Playerctl
    - For volume: alsa-utils, dzen2-xft-xpm
  - stack (for building)
  - tmux + wmctrl (for opening a terminal in the same working directory)
  - xmobar
- Xresources/xrdb
  - dmenu2
  - dzen2-xft-xpm
  - URxvt
- Yay
  - pacman
- Zsh
  - base16-shell (git submodule)
  - find-the-command
  - prezto
  - For Git prompt: Git
  - For Terraform prompt: Terraform
  - For some inline aliases: ripgrep, AWK, curl
  - See _Shell aliases_
- The scripts in the `bin/` folder depend on (possibly Arch Linux-specific)
  programs like
  - alsa-utils
  - base16-shell
  - bc
  - BlueZ
  - curl
  - dmenu2
  - feh
  - gcalcli
  - Ghostscript
  - Git
  - handlr
  - i3lock
  - ImageMagick
  - jq
  - libcanberra
  - libnotify
  - lostfiles
  - mailx
  - NetworkManager
  - pacman
  - pass
  - pip
  - pip-tools
  - Pygments
  - Python 3
  - rustup
  - SoX
  - stack
  - systemd
  - tmux
  - URxvt
  - valgrind
  - VLC
  - wmctrl
  - xclip
  - XMonad
  - xrandr
  - xrdb
  - Yay
  - yeganesh
  - zenity


Color scheme
------------
The color scheme ([base16](https://github.com/chriskempson/base16)-mocha,
[preview](https://emacsthemes.com/themes/base16-mocha-theme.html)) is explicitly
defined in `colors`, `Xresources`, `config/dunst/dunstrc`,
`config/rofi/base16-mocha.rasi`, `config/xmobar/xmobarrc` and
`config/xmonad/xmonad.hs`. The same color scheme is set in `shellrc` through
[`base16-shell`](https://github.com/chriskempson/base16-shell), in
`config/nvim/init.vim` through [a Neovim
plugin](https://github.com/norcalli/nvim-base16.lua) and in `spacemacs` as a
built-in theme. URxvt, dzen and dmenu use the colors defined in `Xresources`.
i3lock and special content in dzen use `colors` to get the appropriate colors.
