(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file) ; or project-find-file-ido
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)

(require 'mk-project)
(setq my-project-source-patterns
      '("*.h" "*.cc" "*.hpp" "*.cpp" "*.s" "*.py" "*.sh" "*.md" "*.txt" "*.cmake"))
(setq my-project-ignore-patterns
      '("*.o" "*.html" "*/build/*" "*build/*" "*build_android/*" " *.d" "#*" "*~" ".pyc"))

(provide 'my-projects)
;;; my-projects.el ends here

