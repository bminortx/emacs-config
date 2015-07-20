## Emacs Customzation ##

My preferred IDE, as customized by me. Despite the fact that it's incredibly uncomfortable for me to edit in any other setup, I feel like this customization is general enough for any new Emacs user to pick up when they're tired of the vanilla setup (which is probably the day of download).

### Setup ###

- Download [Emacs](https://www.gnu.org/software/emacs/emacs.html)
- Clone this repo into `~/.emacs.d/`
- Download all dependencies by running the handy script:

	`~/emacs.d/emacs-config/update-submodules.sh`

- Edit the first line of `~/.emacs` to find the init-emacs file in this repo:

	`(load-file "~/.emacs.d/emacs-config/init-emacs.el")`

- Open Emacs. It'll download all the packages it needs, now that you told it where to look.
- Get on gettin' on!

### Customization and Help ###

The configuration files of this repo are fairly descriptive (my-dev, my-appearance, etc) so look through them if you're curious about what does what for any part of the system. Packages of particular use to me are:

- [Magit](https://github.com/magit/magit) [C-x C-i] - Beautiful Git interface.
- [Dired](http://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html) [C-x C-d] - File management
- [Compilation](http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html) [f6] - Compile and test programs and packages
- [Eshell](http://www.gnu.org/software/emacs/manual/html_mono/eshell.html) [f2] - Shell in Emacs! 

If you're new to Emacs, always remember that you can see all of the keybindings available for your current mode with the keybinding `C-h m`. Such a nice feature.
