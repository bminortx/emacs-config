(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(server-start)

(require 'tramp)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

;; basic initialization, (require) non-ELPA packages, etc.
(setq package-enable-at-startup nil) 
(package-initialize)

(require 'my-packages)
(require 'my-appearance)
(require 'my-moving)
(require 'my-env)
(require 'my-windows)
(require 'my-global-bindings)
(require 'my-custom)
(require 'my-languages)
(require 'my-vcs)
(require 'my-editing)
(require 'my-projects)
(require 'my-fun-packages)
(require 'my-cpplint-fix)

;;;;;;;;;;;;;;;;
;; Desktop Saving and Loading functionality
(provide 'desktop-autosave)
(eval-when-compile
  (require 'cl))
(require 'saveplace)
(setq-default save-place t)
(require 'desktop)
(setq desktop-dirname             "~/.emacs.d/"
      desktop-base-file-name      ".emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname))
(desktop-save-mode 1) ;; Switch on desktop.el
(desktop-load-default)
(desktop-read)
(defun desktop-autosave-save ()
  (desktop-save desktop-dirname))
(add-hook 'auto-save-hook 'desktop-autosave-save)

;;;;;;;;;;;;;;;;
(which-function-mode)
(powerline-default-theme)
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-9"))

(cd "~/")
(provide 'init-emacs)
;;; init-emacs ends here
