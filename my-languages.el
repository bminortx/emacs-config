;;; package --- Summary
;;; Commentary:
;;; Customization of programming language modes

(load-library "google-c-style")
(require 'andersl-cmake-font-lock)
(require 'cmake-mode)
(require 'cpputils-cmake)
(require 'flymake)
(require 'font-lock)
(require 'jedi)
(require 'markdown-mode)
(require 'multi-term)
(require 'paredit)
(require 'protobuf-mode)
(require 'yaml-mode)

;;; Code:

(setq indent-tabs-mode nil)

;;; Comments wrap
(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

;;;;;;;;;;;;;;
;;; C++
(defun brandon-c++-mode ()
  "C++ mode made to fit the way I like it."
  (interactive)
  (c++-mode)
  (subword-mode)
  (google-set-c-style)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)
  (setq-default c-basic-offset 4)
  (c-indent-comment-alist . ((other . (space . 4))))
  )
;;; C++ mode hacks for broken font locking
(defun --copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))
(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
             'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
             'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
             'font-lock-comment-face)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(add-hook 'c++-mode-hook
          '(lambda()
             ;; In theory, we could place some regexes into `c-mode-common-hook'. But note that their
             ;; evaluation order matters.
             (font-lock-add-keywords
              nil '(;; complete some fundamental keywords
                    ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
                    ;; add the new C++11 keywords
                    ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
                    ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
                    ;; PREPROCESSOR_CONSTANT
                    ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
                    ;; hexadecimal numbers
                    ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
                    ;; integer/float/scientific numbers
                    ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
                    ;; ;; user-defined types (customizable)
                    ;; ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
                    ;; some explicit typenames (customizable)
                    ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
                    ))
             ) t)

;;;;;;;;;;;;;;
;;; C
(defun brandon-c-mode ()
  "C mode made to fit the way I like it."
  (interactive)
  (c-mode)
  (subword-mode)
  (google-set-c-style)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)
  )
(c-add-style "my-c-style"
             '("BSD"
	       (c-offsets-alist .((innamespace . 0)))))
(setq-default c-basic-offset 4)
(setq c-doc-comment-style 'javadoc)
(setq whitespace-style (quote (face tabs trailing space-before-tab tab-mark lines)))
(setq windmove-wrap-around t)
;;; Switching between .h and .cc modes
(defun switch-between-h-and-cc ()
  "Switch between a header (.h) and an C++ implementation (.cc/.cpp) file using mk-project support"
  (interactive)
  (let* ((file (file-name-base))
	 (ext (file-name-extension (buffer-file-name)))
	 (header-regex "[h|hpp]")
	 (impl-regex "[cc|cpp|cxx|c]")
	 (newfile (concat file (if (string-match "h" ext)
				   (concat "\." impl-regex)
				 (concat "\." header-regex)))))
    (project-find-file newfile)))
(global-set-key (kbd "M-s") 'switch-between-h-and-cc)
;; (add-hook 'c-mode-common-hook
;; 	  (lambda()
;; 	    (add-hook 'write-contents-functions
;; 		      (lambda()
;; 			(save-excursion
;; 			  (delete-trailing-whitespace))
;; 			(indent-for-tab-command)))))

;;;;;;;;;;;;;;
;;; Python
(defun brandon-python-mode ()
  (interactive)
  (python-mode)
  (subword-mode)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq python-indent-offset 4) 
  (setq compile-command "python ")
  )
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-c\C-k" 'kill-region)))
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)

;;;;;;;;;;;;;;
;;; Objective-C
;;; http://bretthutley.com/programming/emacs/integrating-emacs-and-xcode/
(defun brandon-objc-mode ()
  "C++ mode made to fit the way I like it."
  (interactive)
  (objc-mode)
  (subword-mode)
  (google-set-c-style)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)
  )

(defun bh-choose-header-mode ()
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (progn
        ;; OK, we got a .h file, if a .m file exists we'll assume it's
	;; an objective c file. Otherwise, we'll look for a .cpp file.
        (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
              (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
          (if (file-exists-p dot-m-file)
              (progn
                (objc-mode)
                )
            (if (file-exists-p dot-cpp-file)
                (c++-mode)
              )
            )
          )
        )
    )
  )

(add-hook 'find-file-hook 'bh-choose-header-mode)

;;;;;;;;;;;;;;
;;; Java
(defun brandon-java-mode ()
  (interactive)
  (java-mode)
  (subword-mode) ; Uncomment to treat camelText words as separate
  (google-set-c-style)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)		
  )

;;;;;;;;;;;;;;
;;; Matlab
(matlab-mode)

;;;;;;;;;;;;;;
;;; CUDA
(defun brandon-cuda-mode ()
  (interactive)
  (cuda-mode)
  (google-set-c-style)
  (subword-mode) ; Uncomment to treat camelText words as separate
  (setq tab-width 2)
  (setq-default tab-width 2)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)		
  )

;;;;;;;;;;;;;;
;;; Javascript
(defun brandon-javascript-mode ()
  (interactive)
  (javascript-mode)
  (google-set-c-style)
  (subword-mode) ; Uncomment to treat camelText words as separate
  (setq tab-width 2)
  (setq-default tab-width 2)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)		
  )

;;;;;;;;;;;;;;
;;; HTML
(defun brandon-html-mode ()
  (interactive)
  (html-mode)
  (subword-mode) ; Uncomment to treat camelText words as separate
  (setq tab-width 2)
  (setq-default tab-width 2)
  (which-function-mode 1)
  (setq indent-tabs-mode nil)		
  )

;;;;;;;;;;;;;;
;;; CSS and Sass
(defun brandon-css-mode ()
  (interactive)
  (css-mode)
  (subword-mode) ; Uncomment to treat camelText words as separate
  (which-function-mode 1)
  (setq indent-tabs-mode nil)  
  )

(setq scss-compile-at-save nil) 

;;;;;;;;;;;;;;
;;; Lisp

(setq my-lisp-par-hook #'(lambda ()
			   (paredit-mode 1)
			   (autopair-mode -1)))
(add-hook 'lisp-mode-hook my-lisp-par-hook)
(add-hook 'emacs-lisp-mode-hook my-lisp-par-hook)

;;;;;;;;;;;;;;
;;; Apply all modes
(setq auto-mode-alist (append '(("\\.cpp$" . brandon-c++-mode)
				("\\.cc$" . brandon-c++-mode)
				("\\.hpp$" . brandon-c++-mode)
				("\\.h$" . brandon-c++-mode)
				("\\.m$" . brandon-objc-mode)
				("\\.mm$" . brandon-objc-mode)
				("\\.py$" . brandon-python-mode)
				("\\.java$" . brandon-java-mode)
				("\\.m$" . brandon-matlab-mode)
				("\\.cu$" . brandon-c++-mode)
				("\\.cuh$" . brandon-c++-mode)
				("\\.html$" . brandon-html-mode)
				("\\.css$" . brandon-css-mode)
				("\\.c$" . brandon-c-mode)
				;; For OpenGL Shaders
				("\\.vert$" . brandon-c++-mode)
				("\\.frag$" . brandon-c++-mode)
				("\\.geom$" . brandon-c++-mode)
				("\\.geom_ext$" . brandon-c++-mode)			     
				) auto-mode-alist))

;;;;;;;;;;;;;;
;;; Miscellaneous
(add-hook 'org-mode-hook '(lambda () (visual-line-mode t) ) )
(add-hook 'LaTeX-mode-hook '(lambda ()
			      (visual-line-mode 1)
			      (sentence-highlight-mode)))

(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("Rakefile" . ruby-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))
(autoload 'andersl-cmake-font-lock-activate "andersl-cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'andersl-cmake-font-lock-activate)

;;; cpp utils

(add-hook 'c-mode-hook (lambda () (cppcm-reload-all)))
(add-hook 'c++-mode-hook (lambda () (cppcm-reload-all)))
;;; Configure cpputils-cmake for use with rpg-cmake (def_library/def_executable)
(setq cppcm-cmake-target-regex
      "^\s*[^#]*\s*\\(\\(?:add\\|def\\)_\\(?:executable\\|library\\)\\)\s*(\\(\s*[^\s]+\\)")
(setq cppcm-cmake-exe-regex "^\\(?:def\\|add\\)_executable")
(add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
(setq cppcm-write-flymake-makefile nil)

;;;;;;;;;;;;;;;;
;;; Terminal Settings

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


(provide 'my-languages)
;;; my-languages.el ends here
