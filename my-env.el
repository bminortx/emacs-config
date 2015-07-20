;;; package --- Summary
;;; Commentary:
;;; Settings for both the global (host) environment, and
;;; the Emacs environment

;;; Code:

;;;;;;;;;;;;;;;;;
;;; System Environment: 
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/git/bin"))
(setq exec-path (append exec-path '("/usr/local/git/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/sw/bin"))
(setq exec-path (append exec-path '("/sw/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))
(setq exec-path (append exec-path '("/opt/local/bin")))


;;;;;;;;;;;;;;;;;
;;; Emacs Environment:

;;; Desktop Saving and Loading functionality
(provide 'desktop-autosave)
(eval-when-compile
  (require 'cl))
(require 'saveplace)
(setq-default save-place t)
(require 'desktop)
(setq desktop-dirname             "~/.emacs.d/"
      desktop-base-file-name      ".emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname))
(desktop-save-mode 1) ;; Switch on desktop.el
(desktop-load-default)
(desktop-read)
(defun desktop-autosave-save ()
  (desktop-save desktop-dirname))
(add-hook 'auto-save-hook 'desktop-autosave-save)

(provide 'my-env)
;;; my-env.el ends here
