;; Following code adapted from Emacs-Prelude @
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
(require 'cl-lib)
(defvar my-packages '(
		      auctex
		      auto-complete
		      auto-indent-mode
		      autopair
		      clojure-mode
		      cmake-mode
		      coffee-mode
		      color-theme
		      color-theme-sanityinc-solarized
		      color-theme-sanityinc-tomorrow
		      column-marker
		      dash
		      deft
		      desktop
		      expand-region
		      exec-path-from-shell
		      focus
		      gist
		      groovy-mode
		      guide-key
		      haml-mode
		      handlebars-mode
		      haskell-mode
		      ibuffer
		      ibuffer-vc
		      inf-ruby
		      jedi
		      jenkins-watch
		      magit
		      matlab-mode
		      markdown-mode
		      multi-term
		      mustache-mode
		      notifications
		      paredit
		      popup
		      powerline
		      projectile
		      protobuf-mode
		      rainbow-mode
		      ruby-mode
		      ruby-tools
		      sass-mode
		      scss-mode
		      sentence-highlight
		      solarized-theme
		      twittering-mode
		      virtualenvwrapper
		      volatile-highlights
		      workgroups
		      yaml-mode
		      yari
		      yasnippet
		      )
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'my-packages)
;;; my-packages.el ends here
