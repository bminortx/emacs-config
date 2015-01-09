;;; We define modes for c++, python, and java
;;; Code:
(load-library "google-c-style")
(defun brandon-c++-mode ()
  "C++ mode made to fit the way I like it."
  (interactive)
  (c++-mode)
  (subword-mode)
  (google-set-c-style)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)
  (whitespace-mode 1)
  )

(defun brandon-c-mode ()
  "C mode made to fit the way I like it."
  (interactive)
  (c-mode)
  (subword-mode)
  (google-set-c-style)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)
  (whitespace-mode 1)
  )

(defun brandon-python-mode ()
  (interactive)
  (python-mode)
  (subword-mode)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)
  (setq-default tab-width 2)
  (whitespace-mode 1)
  (setq compile-command "python ")
  )

(defun brandon-java-mode ()
  (interactive)
  (java-mode)
  (subword-mode) ; Uncomment to treat camelText words as separate
  (google-set-c-style)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)		
  (whitespace-mode 1)
  )

(defun brandon-matlab-mode ()
  (interactive)
  (matlab-mode)
  (subword-mode) ; Uncomment to treat camelText words as separate
  (setq tab-width 2)
  (setq-default tab-width 2)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)		
  (whitespace-mode 1)
  )

(defun brandon-cuda-mode ()
  (interactive)
  (cuda-mode)
	(google-set-c-stlye)
  (subword-mode) ; Uncomment to treat camelText words as separate
  (setq tab-width 2)
  (setq-default tab-width 2)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)		
  (whitespace-mode 1)
  )

;;; We set the jack modes as default for the appropraite files
;;; To make this apply only in jack directories add a path to the settings
(setq auto-mode-alist (append '(("\\.cpp$" . brandon-c++-mode)
				("\\.cc$" . brandon-c++-mode)
				("\\.hpp$" . brandon-c++-mode)
				("\\.h$" . brandon-c++-mode)
				("\\.py$" . brandon-python-mode)
				("\\.java$" . brandon-java-mode)
				("\\.m$" . brandon-matlab-mode)
				("\\.cu$" . brandon-cuda-mode)
				("\\.cuh$" . brandon-cuda-mode)
				) auto-mode-alist))

(setq auto-mode-alist (append '(("\\.c$" . brandon-c-mode)) auto-mode-alist))

;; CUSTOM STYLES
(c-add-style "my-c-style"
             '("BSD"
	       (c-offsets-alist .
				((innamespace . 0))
				)))

(setq-default c-basic-offset 4)

(setq c-doc-comment-style 'javadoc)
(setq whitespace-style (quote (face tabs trailing space-before-tab tab-mark lines)))
(setq windmove-wrap-around t)

(provide 'my-custom)
;;; my-custom.el ends here
