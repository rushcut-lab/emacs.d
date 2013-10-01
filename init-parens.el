(require-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)

(sp-with-modes '(feature-mode)
  (sp-local-pair "'" nil :actions nil))

(provide 'init-parens)
