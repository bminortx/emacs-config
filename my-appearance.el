;;; package --- Summary
;;; Commentary:
;;; Colorizes various aspects of the Emacs buffer


(load-file "~/.emacs.d/emacs-config/my-themes/almost-monokai/color-theme-almost-monokai.el")
(require 'ansi-color)
(require 'color-theme)
(require 'color-theme-almost-monokai)
(require 'color-theme-sanityinc-tomorrow)
(require 'color-theme-sanityinc-solarized)
(require 'column-marker)
(require 'focus)

;;; Code:
(color-theme-initialize)

;;; Switch our color with a keybinding
(setq my-color-themes (list	
		       'color-theme-subtle-hacker
		       'color-theme-sanityinc-tomorrow-night
		       'color-theme-sanityinc-solarized-light
		       'color-theme-sanityinc-solarized-dark
		       'color-theme-almost-monokai
		       'color-theme-dark-blue2
		       'color-theme-gnome2
		       'color-theme-gray30
		       'color-theme-late-night
		       'color-theme-retro-green
		       'color-theme-robin-hood))
(defun my-theme-cycle ()
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (funcall (car theme-current))
  (message "%S" (car theme-current)))

;;; Initialization
(setq theme-current my-color-themes)
(setq color-theme-is-global t) 
(define-key global-map [f12] 'my-theme-cycle)
(color-theme-subtle-hacker)

(setq display-buffer-function nil)
(setq display-buffer-reuse-frames t)
(setq pop-up-windows nil)
(setq focus-dimness 1)
(setq whitespace-mode nil)

;;; Colorize compilation
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; Cursor Appearance
(setq default-frame-alist '((cursor-color . "RoyalBlue")
                            (cursor-type . box)
                            (foreground-color . "black")
                            (background-color . "white")))
(setq blink-matching-paren t)
(setq show-paren-delay 0)
(show-paren-mode t)
(setq blink-cursor-mode nil)
(column-number-mode 1)
(add-hook 'c++-mode-hook (lambda () (interactive) (column-marker-1 80)))

(provide 'my-appearance)
;;; my-appearance.el ends here

