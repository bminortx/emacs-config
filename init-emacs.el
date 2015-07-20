;;; package --- Summary
;;; Commentary:
;;; init-emacs.el loads all other packages and configuration files,
;;; then starts Emacs the way we want it.

;;; Code:
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(server-start)

;; basic initialization, (require) non-ELPA packages, etc.
(require 'tramp)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq package-enable-at-startup nil) 
(package-initialize)
(require 'my-packages)
(require 'my-appearance)
(require 'my-moving)
(require 'my-env)
(require 'my-global-bindings)
(require 'my-languages)
(require 'my-vcs)
(require 'my-editing)
(require 'my-projects)
(require 'my-fun-packages)
(require 'my-cpplint-fix)

(which-function-mode)
(powerline-default-theme)
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-9"))

(cd "~/")
(provide 'init-emacs)
;;; init-emacs ends here
