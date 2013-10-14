;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YASNIPPET
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(setq yas/indent-line 'auto)

(add-hook 'yas-minor-mode-hook
          (lambda ()
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            (define-key yas-minor-mode-map [(tab)] nil)))

(global-unset-key (kbd "M-RET"))
(define-key global-map (kbd "M-RET") 'yas-expand)

(yas-global-mode t)

(global-set-key (kbd "C-c y n") `yas-new-snippet)
(global-set-key (kbd "C-c y v") `yas-visit-snippet-file)
(global-set-key (kbd "C-c y f") `yas-find-snippets)
(global-set-key (kbd "C-c y r") `yas-reload-all)

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas/prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))
(setq ac-source-yasnippet nil)

(provide 'init-yasnippet)
