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
        - Statnot
    - Feh (background)
    - Dmenu (keybinding)
    - Urxvt (keybinding)
    - Thunderbird & Firefox (rules for new windows)
- Vim and tmux interact through the M-[hjkl] bindings (see vimrc and tmux.conf)
- IPython
- Git
- Htop
- Systemd
    - Service files for Deluge, Dropbox, redshift and statnot
- Xresources
    - Urxvt
    - Dzen
    - Dmenu
- Bash
- Devilspie

The bin/ folder depends on (possibly Arch-specific) programs like pacman,
highlight, scrot, curl, xclip and VLC.


Colorscheme
-----------

The colorscheme is explicitly defined in .colors and in .Xresources (and in vimrc through a plugin). Urxvt and tmux will use the colors of .Xresources, Herbstluftwm and dzen will read in .colors to get the appropriate colors.
