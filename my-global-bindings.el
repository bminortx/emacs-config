;;; package --- Summary
;;; Commentary:
;;; All of the bindings for global modes (usually)

;;; Code:
;; Typing "yes" or "no" takes too long---use "y" or "n"
(fset 'yes-or-no-p 'y-or-n-p)

;; make option key meta | C-x-C-m = M-x
(setq mac-command-modifier 'meta)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;;;;;;;;;;;;;;;;
;;; Window movement

(require 'ido)
(ido-mode 'both)

(global-set-key "\C-xa" 'windmove-left)
(global-set-key "\C-xs" 'windmove-down)
(global-set-key "\C-xw" 'windmove-up)
(global-set-key "\C-xd" 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<right>") 'windmove-right)

(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1))
(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))
(global-set-key (kbd "<C-S-iso-lefttab>") 'move-cursor-previous-pane)
(global-set-key (kbd "<C-tab>") 'move-cursor-next-pane)

;;;;;;;;;;;;;;;;
;;; Function Keys
(require 'unit-test)
(define-key global-map [f2] 'eshell)
(define-key global-map [f5] 'linum-mode)
(define-key global-map [f6] 'compile)
(global-set-key "\C-x\C-m" 'compile)
(define-key global-map [f7] 'recompile)
(define-key global-map [f8] 'next-error)
(define-key global-map [f9] 'run-unit-tests)

;;;;;;;;;;;;;;;;

(column-number-mode 1)

(setq ring-bell-function 'ignore)

(require 'yasnippet)

(require 'auto-complete)

(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/emacs-config/plugins/auto-complete/dict/")
(require 'auto-complete-config)
(ac-config-default)

(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

;; show paren matches
(setq blink-matching-paren t)
(setq show-paren-delay 0)
(show-paren-mode t)

(global-set-key "\C-xr" 'rgrep)
(setq search-highlight t)	       ; incremental search highlights
(setq query-replace-highlight t)       ; highlight during query

(global-set-key "\C-x\C-r" 'revert-buffer)

(setq blink-cursor-mode nil)

(put 'upcase-region 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)

(put 'downcase-region 'disabled nil)

(global-set-key "\M-o" 'project-find-file-ido)

(global-set-key [?\C-,] (lambda () (interactive) (scroll-up 1)))
(global-set-key [?\C-.] (lambda () (interactive) (scroll-down 1)))

(require 'multi-term)
(add-hook 'term-mode-hook
  (lambda()
    (local-unset-key (kbd "<tab>"))))
(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil)
  (set (make-local-variable 'global-hl-line-mode) nil)
  (ad-activate 'term-char-mode)
  (term-set-escape-char ?\C-x))
(unwind-protect
    (global-set-key (kbd "<XF86Launch5>") 'multi-term))

(setq
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
    "^\*GTAGS" "^session\.*" "^\*" "^\*magit-process\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~/src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion
(defun insert-doc-comment ()
  (interactive)
  (insert "/**  */")
  (dotimes (number 3) (backward-char)))

(global-set-key (kbd "M-:") 'insert-doc-comment)
(global-set-key "\C-x\C-d" 'dired)

(setq compilation-always-kill t)
(setq compilation-skip-threshold 2)
(global-set-key (kbd "<f2>") 'multi-term)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'my-global-bindings)
;;; my-global-bindings.el ends here
