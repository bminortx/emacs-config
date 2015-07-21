;;; package --- Summary
;;; Commentary:
;;; All of the bindings for global modes (usually)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'unit-test)
(require 'yasnippet)

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
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/emacs-config/plugins/auto-complete/dict/")
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key "\C-x\C-r" 'revert-buffer)

(provide 'my-global-bindings)
;;; my-global-bindings.el ends here
