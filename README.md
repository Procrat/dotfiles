Procrat's dotfiles
==================

Some parts are dependent on each other, so be careful when blindly copying
certain parts.


Dependency tree
---------------

- Alacritty
- compton
- dunst
- feh
- Git
- htop
- IPython
- NeoVim
  - NeoVim and tmux interact through the M-[hjkl] bindings (see `vimrc` and `tmux.conf`)
  - vim-plug
  - For all plugins, see `vimrc`
  - ag
  - xclip or xsel for synchronisation with X11 clipboard
  - universal-ctags
  - hdevtools
  - hlint
  - flake8
  - pylint
  - racer
  - clang
  - isort
- pylint
- ranger
- systemd
  - Service files for tmux and redshift
- tern
- tmux
  - TPM (Tmux Plugin Manager)
  - NeoVim and tmux interact through the M-[hjkl] bindings (see `vimrc` and `tmux.conf`)
  - xclip
- XMonad
  - [Procrat/XMonad.Layout.SingleSpacing](https://github.com/Procrat/xmonad-singlespacing) (git submodule)
  - [Procrat/XMonad.Actions.Contexts](https://github.com/Procrat/xmonad-contexts) (git submodule)
    - yeganesh
    - dmenu
  - dzen
    - alsa-utils
    - Icons (defined in config/icons/)
    - wmctrl
    - xorg-xrandr
    - xdg-utils (for xdg-open)
    - Dropbox
  - Launchers
    - urxvt
    - xterm
    - rofi-pass
    - xdg-utils (for xdg-open)
    - j4-dmenu-desktop
  - Media keys
    - xbacklight
    - alsa-utils
    - playerctl
- Xresources
  - urxvt
  - dzen
  - dmenu
- yaourt
- zsh
  - prezto
  - base16-shell (git submodule)
- The scripts in the `bin/` folder depends on (possibly Arch-specific) programs
  like
  - alsa-utils
  - base16-shell
  - bc
  - bluez
  - curl
  - diff
  - dmenu
  - emacs
  - escrotum
  - feh
  - i3lock
  - imagemagick
  - libcanberra
  - libnotify
  - mpv
  - openldap
  - pacman
  - pass
  - pip
  - pulseaudio
  - pygmentize
  - rclone
  - sendmail
  - sox
  - systemd
  - tmux
  - valgrind
  - vlc
  - wmctrl
  - xclip
  - XMonad
  - xorg-xrdb
  - yaourt
  - yeganesh

### Outdated
- Bash
  - Base16-shell (git submodule)
- Spacemacs
- Herbstluftwm
    - Dzen (bar)
        - Alsa-utils (volume manager)
        - Icons (defined in .config/icons/)
    - Feh (background)
    - Dmenu (keybinding)
    - URxvt (keybinding)


Color scheme
------------
The color scheme is explicitly defined in `colors`, `Xresources`,
`config/dunst/dunstrc`, `config/alacritty/alacritty.yml` and
`xmonad/xmonad.hs`. `shellrc` and in `vimrc` also set the same color scheme
through a plugin. URxvt, dzen and dmenu use the colors defined in `Xresources`.
Herbstluftwm and special content in dzen use `colors` to get the appropriate
colors.
