Procrat's dotfiles
==================

Some parts are dependent on eachother, so be careful when blindly copying
certain parts.


Dependency tree
---------------

- Herbstluftwm
    - Dzen (bar)
        - Alsa-utils (volume manager)
        - Icons (defined in .config/icons/)
    - Feh (background)
    - Dmenu (keybinding)
    - Urxvt (keybinding)
- (Neo)Vim
    - Vundle (will be installed by setup.sh)
    - Ag
    - YouCompleteMe libraries
    - Python packages
        - isort for vim-isort
        - flake8 for khuno.vim
        - jad for JavaDecompiler.vim
- Tmux
- (Neo)Vim and tmux interact through the M-[hjkl] bindings (see vimrc and tmux.conf)
- IPython
- Git
- Htop
- Systemd
    - Service files for Deluge, Dropbox and redshift
- Xresources
    - Urxvt
    - Dzen
    - Dmenu
- Bash
- Zsh
    - Prezto

The bin/ folder depends on (possibly Arch-specific) programs like pacman,
highlight, scrot, curl, xclip and VLC.


Colorscheme
-----------

The colorscheme is explicitly defined in .colors and in .Xresources (and in
vimrc through a plugin). Urxvt and tmux will use the colors of .Xresources,
Herbstluftwm and dzen will read in .colors to get the appropriate colors.
