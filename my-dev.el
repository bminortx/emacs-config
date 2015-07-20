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
;;; Compile
;;; See also my-global-bindings:function keys
(global-set-key "\C-x\C-m" 'compile)
(setq compilation-always-kill t)
(setq compilation-skip-threshold 2)

;;;;;;;;;;;;;;;;
;;; GDB
;; Avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
		'(lambda ()(interactive) (gud-gdb (concat "gdb --fullname "
							  (cppcm-get-exe-path-current-buffer)))))


(provide 'my-dev)
;;; my-dev.el ends here
