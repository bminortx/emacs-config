;;; package --- Summary
;;; Commentary:
;;; init-emacs.el loads all other packages and configuration files,
;;; then starts Emacs the way we want it.

(require 'tramp)
(require 'package)

;;; Code:
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(server-start)

;; basic initialization, (require) non-ELPA packages, etc.
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq package-enable-at-startup nil) 
(package-initialize)
(require 'my-packages)
(require 'my-env)
(require 'my-appearance)
(require 'my-global-bindings)
(require 'my-moving)
(require 'my-languages)
(require 'my-editing)
(require 'my-dev)
(require 'my-cpplint-fix)

(which-function-mode)
(powerline-default-theme)
(add-to-list 'default-frame-alist '(font . "Hack-7.5"))

(cd "~/")
(provide 'init-emacs)
;;; init-emacs ends here
