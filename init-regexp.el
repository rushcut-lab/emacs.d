(require-package 'visual-regexp)
;; (require-package 'visual-regexp-steroids)

;; TODO: Study

(define-key esc-map (kbd "C-r") 'vr/replace)        ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/query-replace)  ;; C-M-s

;; (global-unset-key (kbd "C-r"))
;; (global-unset-key (kbd "C-s"))

;; (global-set-key (kbd "C-r") 'vr/isearch-backward)
;; (global-set-key (kbd "C-s") 'vr/isearch-forward)

(provide 'init-regexp)
