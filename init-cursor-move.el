(require-package 'ace-jump-mode)
(require-package 'key-chord)

(key-chord-mode t)

(key-chord-define-global "fj" 'ace-jump-mode)

(provide 'init-cursor-move)
