(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default
 inhibit-startup-message t
 inhibit-splash-screen t
 confirm-kill-emacs 'yes-or-no-p
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 compilation-scroll-output t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 tab-width 2
 line-spacing 0
 mouse-yank-at-point nil
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell t
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BACKUP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Place all backup files into this directory
(custom-set-variables
 '(auto-save-interval 300)
 '(auto-save-timeout 10)
 '(backup-directory-alist (list (cons "." (expand-file-name "backup" user-emacs-directory))))
 '(backup-by-copying t)
 '(delete-old-versions t)
 '(kept-new-versions 20)
 '(kept-old-versions 2)
 '(vc-make-backup-files t)
 '(version-control t)
 '(delete-by-moving-to-trash t))

(defun init--force-backup ()
  "Reset backed up flag."
  (setq buffer-backed-up nil))

(add-hook 'auto-save-hook 'init--force-backup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; META-KEYS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq default-input-method "MacOSX")
  (setq mouse-wheel-scroll-amount '(0.001)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BACKGROUND for iTERM
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *is-cocoa-emacs*
  (defun focus-iterm ()
    (interactive)
    (shell-command "osascript -e 'tell application \"iTerm\" to activate'"))

  (if window-system
      (progn (global-unset-key (kbd "C-z"))
             (global-unset-key (kbd "C-x C-z"))
             (global-set-key (kbd "C-z") 'focus-iterm)))
  )

(provide 'init-emacs-preference)
