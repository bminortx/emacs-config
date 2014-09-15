;;; Settings for the fun packages that I've installed
;;; They don't help with programming, but they're neat!

;; Twittering Mode options
(setq twittering-use-icon-storage t)
(setq twittering-icon-mode t)
(setq twittering-status-format
      "%FOLD{%RT{%FACE[bold]{RT}}%i%s>>%r @%C{%Y-%m-%d %H:%M:%S} %@{}\n%FOLD[ ]{%T%RT{\nretweeted by %s @%C{%Y-%m-%d %H:%M:%S}}}}")

;; Pianobar options
(setq pianobar-username "")
(setq pianobar-password "")
(setq pianobar-key [ctrl+f1])

(provide 'my-fun-packages)
;;; my-fun-packages.el ends here
