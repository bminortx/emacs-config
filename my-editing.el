;; Turn on selection
(setq transient-mark-mode 't highlight-nonselected-windows 't)

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

(eval-after-load "ispell"
   (progn
     (setq ispell-extra-args '()
           ispell-silently-savep t
           ispell-dictionary "american")))

(setq-default ispell-program-name "ispell")

(require 'autopair)
(add-hook 'term-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))

(add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

;; Major life saver --- auto-indents yanked code
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

(setq compilation-skip-threshold 0)
(add-hook 'c-mode-common-hook
	  (lambda()
	    (add-hook 'write-contents-functions
		      (lambda()
			(save-excursion
			  (delete-trailing-whitespace))
			(indent-for-tab-command)))))

(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

(global-set-key (kbd "C-M-y") 'replace-rectangle)

(require 'column-marker)
(add-hook 'c++-mode-hook (lambda () (interactive) (column-marker-1 80)))

(require 'paredit)
(setq my-lisp-par-hook #'(lambda ()
			   (paredit-mode 1)
			   (autopair-mode -1)))
(add-hook 'lisp-mode-hook my-lisp-par-hook)
(add-hook 'emacs-lisp-mode-hook my-lisp-par-hook)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'anything)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "EDITOR"))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/home/replica/RepLabs/server/venv/")

(provide 'my-editing)

;;; my-editing.el ends here
