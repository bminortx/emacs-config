;;; package --- Summary
;;; Commentary:
;;; The common development functionality for Emacs (git, testing, etc)

;;; Code:

;;;;;;;;;;;;;;;;
;;; Git
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'magit)
(global-set-key "\C-x\C-i" 'magit-status)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;;;;;;;;;;;;;;;;
;;; Alarms
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;
;;; Compilation
;;; See also my-global-bindings:function keys
(global-set-key "\C-x\C-m" 'compile)
(setq compilation-always-kill t)
(setq compilation-skip-threshold 0)

(require 'notifications)
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
	(delete-windows-on buffer)
	;; Notify on compilation success
	(notifications-notify
	 :title "Compilation Success"
	 :timeout 2000
	 :urgency 'low))
    (notifications-notify
     ;; Notify on compilation failure
     :title "Compilation Failure"
     :timeout 2000
     :urgency 'low))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame)
  )

(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

;;;;;;;;;;;;;;;;
;;; GDB
;; Avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
		'(lambda ()(interactive) (gud-gdb (concat "gdb --fullname "
							  (cppcm-get-exe-path-current-buffer)))))

(require 'anything)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "EDITOR"))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/home/replica/RepLabs/server/venv/")


(provide 'my-dev)
;;; my-dev.el ends here
