Procrat's dotfiles
==================

The entrypoint to everything is `setup.sh` which installs all dotfiles, including
their dependencies. Most functions in there should usable by themselves without
external dependencies unless otherwise mentioned.


Color scheme
------------
The color scheme ([base16](https://github.com/chriskempson/base16)-mocha,
[preview](https://emacsthemes.com/themes/base16-mocha-theme.html)) is explicitly
defined in `colors`, `Xresources`, `config/alacritty/alacritty.yml`,
`config/dunst/dunstrc`, `config/rofi/base16-mocha.rasi`,
`config/xmobar/xmobarrc` and `config/xmonad/xmonad.hs`. It's also set in
`config/nvim/init.vim` through [a Neovim
plugin](https://github.com/norcalli/nvim-base16.lua) and in `spacemacs` as a
built-in theme. Dzen uses the colors defined in `Xresources`. i3lock uses
`colors`.
