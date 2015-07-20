## Emacs Customzation ##

My preferred IDE, as customized by me. Despite the fact that it's incredibly uncomfortable for me to edit in any other setup, I feel like this customization is general enough for any Emacs user to pick up when they're tired of the vanilla setup (which is probably the day of download).

### Setup ###

- Download [Emacs](https://www.gnu.org/software/emacs/emacs.html)
- Clone this repo into ~/.emacs.d/
- Edit the first line of ~/.emacs to find the init-emacs file in this repo:

	(load-file "~/.emacs.d/emacs-config/init-emacs.el")

- Download all dependencies by running the handy script:

	~/emacs.d/emacs-config/update-submodules.sh

- Open Emacs. It'll download all the packages it needs
- You should be ready to go!

### Customization and Help ###

The configuration files of this repo are fairly descriptive (my-dev, my-appearance, etc) so look through them if you're curious about

Note: If you're new to Emacs, always remember that you can see all of the keybindings available for your current mode with the keybinding `C-h m`. Such a nice feature.
