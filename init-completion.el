;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AUTO COMPLETE MODE
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'popup)
(require-package 'auto-complete)
(require 'auto-complete-config)
;; (global-auto-complete-mode nil)

(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))

(ac-config-default)
(setq ac-auto-start 2
      ac-dwim t
      ac-ignore-case nil
      ac-quick-help-delay 1
      ac-fuzzy-enable t
      ac-use-fuzzzy t
      ac-auto-show-menu 1)

(dolist
    (mode
     '(emacs-lisp-mode lisp-interaction-mode
                       magit-log-edit-mode log-edit-mode org-mode text-mode
                       haml-mode sass-mode yaml-mode csv-mode espresso-mode
                       haskell-mode html-mode nxml-mode sh-mode smarty-mode
                       clojure-mode lisp-mode textile-mode markdown-mode
                       tuareg-mode cperl-mode sass-mode ncl-mode latex-mode
                       fortran-mode f90-mode))
  (add-to-list 'ac-modes mode))

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(defun ac-exit-to-normal-state
  "Stops completing and returns to normal state"
  (interactive "")
  (ac-stop))

(fill-keymap ac-completing-map
             "C-[" 'ac-exit-to-normal-state
             "C-l" 'ac-expand-common
             "C-n" 'ac-next
             "C-s" 'ac-isearch
             "C-p" 'ac-previous
             "C-g" 'ac-stop
             "ESC" 'ac-stop)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIPPIE
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line
                                            try-expand-line-all-buffers)))
    (hippie-expand nil)))

(global-set-key (kbd "M-o") 'hippie-expand-lines)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

(provide 'init-completion)
