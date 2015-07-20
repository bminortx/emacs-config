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

;;; Colorize compilation
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; Cursor Appearance 
(setq blink-matching-paren t)
(setq show-paren-delay 0)
(show-paren-mode t)
(setq blink-cursor-mode nil)
(column-number-mode 1)

;;; Comments
(set-face-foreground font-lock-comment-face "OrangeRed3")
(set-face-foreground font-lock-comment-delimiter-face "OrangeRed4")
(set-face-foreground font-lock-doc-face "OrangeRed3")

;;; Color of Buffer at Init
(load-theme 'sanityinc-tomorrow-day t)

(provide 'my-appearance)
;;; my-appearance.el ends here

