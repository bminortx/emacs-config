;;; package --- Summary
;;; Commentary:
;; Colorizes various aspects of the Emacs buffer

;;; Code:
;;; All of our themes
(load-file "~/.emacs.d/emacs-config/my-themes/almost-monokai/color-theme-almost-monokai.el")
(require 'color-theme)
(require 'color-theme-solarized)
(require 'color-theme-almost-monokai)
(require 'color-theme-sanityinc-tomorrow)

;; Turn on pretty font colors
(global-font-lock-mode t)
(setq font-lock-maximum-decoration 3)
(setq frame-title-format
      '(:eval
        (if buffer-file-name
            (replace-regexp-in-string
             (getenv "HOME") "~"
             (concat (file-name-directory buffer-file-name) "%b") )
          (buffer-name)
          )))
(setq display-buffer-function nil)
(setq display-buffer-reuse-frames t)
(setq pop-up-windows nil)

;;; Colorize compilation
(require 'ansi-color)
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
(require 'column-marker)
(add-hook 'c++-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; Modeline Appearance
(set-face-background 'mode-line "DarkOliveGreen2")
(set-face-foreground 'mode-line "black")

;;; Comments
(set-face-foreground font-lock-comment-face "OrangeRed3")
(set-face-foreground font-lock-comment-delimiter-face "OrangeRed4")
(set-face-foreground font-lock-doc-face "OrangeRed3")

;;; Color Theme of Buffer at Init
(load-theme 'sanityinc-tomorrow-day t)

(provide 'my-appearance)
;;; my-appearance.el ends here

