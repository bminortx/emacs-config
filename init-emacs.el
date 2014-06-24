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
(require 'my-colors)
(require 'my-moving)
(require 'my-env)
(require 'my-windows)
(require 'my-interaction)
(require 'my-custom)
(require 'my-languages)
(require 'my-vcs)
(require 'my-editing)
(require 'my-projects)
(require 'my-fun-packages)

(which-function-mode)
(color-theme-solarized-dark)
(powerline-default-theme)

;; Add Google C++ Style Checker
;; In default, syntax checked by Clang and Cppcheck.
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     (flycheck-add-next-checker 'c/c++-clang
				'c/c++-googlelint 'append)
     )
  )
(setq auto-mode-alist (cons '("\.v$" . coq-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\.v$" . coq-mode) auto-mode-alist))

;; Coq editing mode
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(load-file "~/.emacs.d/ProofGeneral-4.2/ProofGeneral-4.2/generic/proof-site.el")
;; Pianobar
(add-to-list 'load-path "/home/agrif/emacsinclude")
(autoload 'pianobar "pianobar" nil t)
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))


(cd "~/")
