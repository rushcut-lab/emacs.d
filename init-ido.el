;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ido-mode +1)
(ido-load-history)

(custom-set-variables
 '(ido-enable-regexp nil)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-read-file-name-as-directory-commands nil)
 '(ido-use-filename-at-point nil))
(put 'ido-exit-minibuffer 'ido 'ignore)

(defun init--ido-setup ()
  (define-key ido-completion-map (kbd "M-m") 'ido-merge-work-directories)
  (define-key ido-completion-map "\C-c" 'ido-toggle-case))
(add-hook 'ido-setup-hook 'init--ido-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'ido-complete-space-or-hyphen)
;; (ido-complete-space-or-hyphen-enable)

(require-package 'ido-vertical-mode)
(ido-vertical-mode +1)

(require-package 'smex)
(global-set-key (kbd "M-x") 'smex)

;; (require 'ido-yes-or-no)
;; (autoload 'ido-yes-or-no-p "ido-yes-or-no")

(provide 'init-ido)
