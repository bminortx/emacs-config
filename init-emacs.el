(if (string= system-type "darwin")
    (progn (cd "/Applications/Emacs.app/")
	   (normal-top-level-add-subdirs-to-load-path)
	   (setenv "WEBOTS_HOME" "/Applications/Webots/"))
  )
(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
(server-start)

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(TeX-PDF-mode t)
;;  '(ac-quick-help-prefer-x nil)
;;  '(asm-comment-char 35)
;;  '(compilation-scroll-output t)
;;  '(compilation-skip-threshold 2)
;;  '(ecb-options-version "2.32")
;;  '(font-lock-global-modes t)
;;  '(font-lock-support-mode (quote jit-lock-mode))
;;  '(garak-hide-offline-buddies t)
;;  '(grep-command "git grep -nH -e ")
;;  '(grep-highlight-matches t)
;;  '(grep-template "git grep <C> -nH -e <R> <F>")
;;  '(grep-use-null-device nil)
;;  '(gud-gdb-command-name "gdb --annotate=1")
;;  '(inhibit-startup-screen t)
;;  '(initial-buffer-choice nil)
;;  '(inverse-video nil)
;;  '(ispell-dictionary "australian" t)
;;  '(ispell-highlight-face (quote flyspell-incorrect))
;;  '(large-file-warning-threshold nil)
;;  '(left-margin 0)
;;  '(magit-log-cutoff-length 100)
;;  '(magit-status-buffer-switch-function (quote switch-to-buffer))
;;  '(mk-proj-use-ido-selection t)
;;  '(mode-line-format (quote ("%e" #("-" 0 1 (help-echo "mouse-1: Select (drag to resize)
;; mouse-2: Make current window occupy the whole frame
;; mouse-3: Remove current window from display")) mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification #("   " 0 3 (help-echo "mouse-1: Select (drag to resize)
;; mouse-2: Make current window occupy the whole frame
;; mouse-3: Remove current window from display")) mode-line-position (vc-mode vc-mode) #("  " 0 2 (help-echo "mouse-1: Select (drag to resize)
;; mouse-2: Make current window occupy the whole frame
;; mouse-3: Remove current window from display")) (which-func-mode ("" which-func-format #("--" 0 2 (help-echo "mouse-1: Select (drag to resize)
;; mouse-2: Make current window occupy the whole frame
;; mouse-3: Remove current window from display")))) #("-%-" 0 3 (help-echo "mouse-1: Select (drag to resize)
;; mouse-2: Make current window occupy the whole frame
;; mouse-3: Remove current window from display")))))
;;  '(org-export-headline-levels 1)
;;  '(org-export-with-toc nil)
;;  '(org-hide-leading-stars t)
;;  '(org-odd-levels-only t)
;;  '(overline-margin 2)
;;  '(safe-local-variable-values (quote ((outline-minor-mode))))
;;  '(semantic-stickyfunc-indent-string "   ")
;;  '(show-paren-mode t)
;;  '(size-indication-mode nil)
;;  '(todochiku-command "/usr/bin/notify-send")
;;  '(todochiku-tooltip-too nil)
;;  '(whitespace-style (quote (tabs trailing lines)))
;;  '(x-gtk-whole-detached-tool-bar nil)
;;  '(yas/next-field-key (quote ("<C-tab>")) t)
;;  '(yas/prompt-functions (quote (yas/dropdown-prompt yas/completing-prompt yas/ido-prompt yas/no-prompt)))
;;  '(yas/trigger-key "<C-tab>" t))

;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(Font-Lock-String-face ((t (:foreground "forest green" :slant italic))))
;;  '(background "blue")
;;  '(bold ((t (:weight bold))))
;;  '(compilation-error ((t (:foreground "red" :underline t :weight bold))))
;;  '(compilation-warning ((((class color) (min-colors 16)) (:background "lightblue1" :foreground "Orange" :weight bold))))
;;  '(cursor ((t (:background "SteelBlue"))))
;;  '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
;;  '(font-lock-comment-face ((t (:foreground "firebrick"))))
;;  '(font-lock-constant-face ((t (:foreground "steel blue"))))
;;  '(font-lock-doc-string-face ((t (:foreground "green2"))) t)
;;  '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
;;  '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
;;  '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
;;  '(font-lock-reference-face ((t (:foreground "DodgerBlue"))) t)
;;  '(font-lock-type-face ((t (:stipple nil :background "#ffffff" :foreground "LimeGreen" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :width normal))))
;;  '(font-lock-variable-name-face ((t (:foreground "PaleGreen"))))
;;  '(font-lock-warning-face ((nil (:foreground "red"))))
;;  '(fringe ((t (:background "white" :foreground "white"))))
;;  '(highlight ((t (:background "LightSkyBlue"))))
;;  '(linum ((t (:foreground "gray30"))))
;;  '(list-mode-item-selected ((t (:background "gold"))))
;;  '(magit-diff-add ((nil (:foreground "blue"))))
;;  '(magit-item-highlight ((((class color) (background light)) (:background "gray90"))))
;;  '(makefile-space ((t (:background "wheat"))))
;;  '(makefile-space-face ((t (:background "wheat"))) t)
;;  '(mode-line ((t (:background "lawn green"))))
;;  '(mode-line-inactive ((t (:background "gray" :foreground "black"))))
;;  '(org-tag ((t (:slant oblique :weight bold))))
;;  '(outline-4 ((t (:foreground "Brown"))))
;;  '(paren-match ((t (:background "darkseagreen4"))))
;;  '(region ((t (:background "DarkSlateBlue"))))
;;  '(scroll-bar ((t nil)))
;;  '(show-paren-mismatch ((((class color)) (:foreground "white" :background "red"))))
;;  '(speedbar-button-face ((((class color) (background dark)) (:foreground "green4"))))
;;  '(speedbar-directory-face ((((class color) (background dark)) (:foreground "khaki"))))
;;  '(speedbar-file-face ((((class color) (background dark)) (:foreground "cyan"))))
;;  '(speedbar-tag-face ((((class color) (background dark)) (:foreground "Springgreen"))))
;;  '(tool-bar ((t (:foreground "black" :box (:line-width 1 :style released-button)))))
;;  '(vertical-border ((t (:background "DarkOliveGreen2" :foreground "black"))))
;;  '(vhdl-speedbar-architecture-selected-face ((((class color) (background dark)) (:underline t :foreground "Blue"))))
;;  '(vhdl-speedbar-entity-face ((((class color) (background dark)) (:foreground "darkGreen"))))
;;  '(vhdl-speedbar-entity-selected-face ((((class color) (background dark)) (:underline t :foreground "darkGreen"))))
;;  '(vhdl-speedbar-package-face ((((class color) (background dark)) (:foreground "black"))))
;;  '(vhdl-speedbar-package-selected-face ((((class color) (background dark)) (:underline t :foreground "black"))))
;;  '(which-func ((((class color) (min-colors 88) (background light)) (:inherit (mode-line)))))
;;  '(widget-field ((((class grayscale color) (background light)) (:background "LightBlue")))))

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

(cd "~/")


