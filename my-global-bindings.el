;;; package --- Summary
;;; Commentary:
;;; All of the bindings for global modes (usually)

;;; Code:
;; Typing "yes" or "no" takes too long---use "y" or "n"
(fset 'yes-or-no-p 'y-or-n-p)

;; make option key meta | C-x-C-m = M-x
(setq mac-command-modifier 'meta)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

;;;;;;;;;;;;;;;;
;;; Function Keys
(require 'unit-test)
(define-key global-map [f2] 'eshell)  ;; Use multi-term, alternatively
(define-key global-map [f5] 'linum-mode)
(define-key global-map [f6] 'compile)
(define-key global-map [f7] 'recompile)
(define-key global-map [f8] 'next-error)
(define-key global-map [f9] 'run-unit-tests)
(define-key global-map [f11] 'toggle-fullscreen)

;;;;;;;;;;;;;;;;
;;; Auto-complete
;;; Note: Not set up, but would be useful
(require 'yasnippet)
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/emacs-config/plugins/auto-complete/dict/")
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

;;;;;;;;;;;
;;; Searching Files - grep, search
(global-set-key "\C-xr" 'rgrep)
(setq search-highlight t)	       ; incremental search highlights
(setq query-replace-highlight t)       ; highlight during query

;;; Searching Buffers - ido, ibuffer, dired
(require 'ido)
(ido-mode 'both)
(global-set-key "\M-o" 'project-find-file-ido)
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
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-x\C-d" 'dired)
(put 'dired-find-alternate-file 'disabled nil)

(defun insert-doc-comment ()
  (interactive)
  (insert "/**  */")
  (dotimes (number 3) (backward-char)))
(global-set-key (kbd "M-:") 'insert-doc-comment)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key "\C-x\C-r" 'revert-buffer)

(provide 'my-global-bindings)
;;; my-global-bindings.el ends here
