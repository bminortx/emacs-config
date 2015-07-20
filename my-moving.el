;;; package --- Summary
;;; Commentary: 
;;----Moving around---

;;; Code:

;;;;;;;;;;;;;;;;
;;; Window Movement

(global-set-key "\C-xa" 'windmove-left)
(global-set-key "\C-xs" 'windmove-down)
(global-set-key "\C-xw" 'windmove-up)
(global-set-key "\C-xd" 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<right>") 'windmove-right)
(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1))
(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))
;;; Just like in Chrome! Woo
(global-set-key (kbd "<C-S-iso-lefttab>") 'move-cursor-previous-pane)
(global-set-key (kbd "<C-tab>") 'move-cursor-next-pane)
;;; Save your windows, if you want
(require 'workgroups)
(workgroups-mode 1)
(setq wg-prefix-key (kbd "C-z"))
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;;;;;;;;;;;;;;
;;; Frame Movement

;; Put scroll bar on the right side
(set-scroll-bar-mode 'right)
(set-scroll-bar-mode nil)
;; Remove graphical toolbar (if version 21 or higher)
(tool-bar-mode 0)
;; don't show tooltips
(tooltip-mode 0)
;; use mouse wheel to scroll
(mouse-wheel-mode t)
;; Scroll one line at a time
(setq scroll-step 1)
;; Default: scroll with middle,  page-up with left click, page down with right
;; Scroll with left mouse button
;; This only works with the plain scroll bar, not the Xaw3d variety
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)
;; Page down with middle button
(global-set-key [vertical-scroll-bar mouse-2] 'scroll-bar-scroll-up)
(global-set-key [vertical-scroll-bar drag-mouse-2] 'scroll-bar-scroll-up)
(global-unset-key [vertical-scroll-bar down-mouse-2])
;; Page up with right button
(global-set-key [vertical-scroll-bar mouse-3] 'scroll-bar-scroll-down)
(global-set-key [vertical-scroll-bar drag-mouse-3] 'scroll-bar-scroll-down)
(global-set-key [?\C-,] (lambda () (interactive) (scroll-up 1)))
(global-set-key [?\C-.] (lambda () (interactive) (scroll-down 1)))
(global-set-key [end] 'end-of-buffer) 
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [C-right] 'forward-word)
(global-set-key [C-left] 'backward-word)
;; Scroll faster, colors won't appear correctly right away
(setq lazy-lock-defer-on-scrolling t)
(setq lazy-lock-continuity-time 0.3)
;; Don't insert new lines when scrolling
(setq next-line-add-newlines nil)

(provide 'my-moving)
