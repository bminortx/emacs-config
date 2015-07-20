;; Turn on selection
(setq transient-mark-mode 't highlight-nonselected-windows 't)

;;;;;;;;;;;;;;;;
;;; Search and Replace

;;; Files
(global-set-key "\C-xr" 'rgrep)
(setq search-highlight t)	       ; incremental search highlights
(setq query-replace-highlight t)       ; highlight during query
;; Pressing backspace during an Isearch will delete the previous
;; character typed (or do a reverse isearch if something matches the
;; current word).  Without this, it will delete the highlighted text.
(define-key isearch-mode-map [backspace] 'isearch-delete-char)
;; Replace highlighted/marked areas
(delete-selection-mode t)
(setq indent-tabs-mode nil)
(global-unset-key "\C-c\C-c")
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)
(global-set-key "\C-ck" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-w" 'backward-kill-word)
(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "C-M-y") 'replace-rectangle)

;;; Buffers - ido, ibuffer, dired
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

(insert "/**  */")
(global-set-key (kbd "M-:") 'insert-doc-comment)

;;;;;;;;;;;;;;;;
;;; Copy and Paste

(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
		 (message "Copied line")
		 (list (line-beginning-position)
		       (line-beginning-position 2)))))
(defadvice yank (before slick-copy activate)
  "Position point when yanking lines."
  (let ((kill (current-kill 0 t)))
    (when (eq ?\n (elt kill (1- (length kill))))
      (beginning-of-line))))

;;;;;;;;;;;;;;;;
;;; Spellcheck
(eval-after-load "ispell"
   (progn
     (setq ispell-extra-args '()
           ispell-silently-savep t
           ispell-dictionary "american")))
(setq-default ispell-program-name "ispell")

;;;;;;;;;;;;;;;;
;;; Automatic functionality

;;; Automatically add braces and parens
(require 'autopair)
(add-hook 'term-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

;; Auto-indents yanked code
(dolist (command '(yank yank-pop))
       (eval `(defadvice ,command (after indent-region activate)
                (and (not current-prefix-arg)
                     (member major-mode '(emacs-lisp-mode lisp-mode
                                                          clojure-mode    scheme-mode
                                                          haskell-mode    ruby-mode
                                                          rspec-mode      python-mode
                                                          c-mode          c++-mode
                                                          objc-mode       latex-mode
                                                          plain-tex-mode))
                     (let ((mark-even-if-inactive transient-mark-mode))
                       (indent-region (region-beginning) (region-end) nil))))))

(provide 'my-editing)

;;; my-editing.el ends here
