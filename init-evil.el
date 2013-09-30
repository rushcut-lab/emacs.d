(setq evil-toggle-key "C-M-\\")
(require-package 'evil)

(evil-mode 1)
(setq evil-default-cursor t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; surround
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'surround)
(global-surround-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil-numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(setq evil-normal-state-tag   (propertize " N " 'face '((:background "brown" :foreground "white")))
      evil-emacs-state-tag    (propertize " E " 'face '((:background "red" :foreground "white")))
      evil-insert-state-tag   (propertize " I " 'face '((:background "YellowGreen" :foreground "white")))
      evil-motion-state-tag   (propertize " M " 'face '((:background "blue")))
      evil-visual-state-tag   (propertize " V " 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize " O " 'face '((:background "purple")))
      evil-normal-state-cursor `(box "Orange")
      evil-motion-state-cursor `(box "Red")
      evil-insert-state-cursor '(bar "YellowGreen")
      evil-emacs-state-cursor  '(bar "YellowGreen")
      evil-visual-state-cursor `(box ,(face-attribute 'font-lock-keyword-face :foreground))
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-normal-state-map "  " 'ace-jump-mode)
(define-key evil-normal-state-map " k" 'ace-jump-char-mode)
(define-key evil-normal-state-map " l" 'ace-jump-line-mode)
(define-key evil-normal-state-map " s" 'textmate-goto-symbol)
(define-key evil-normal-state-map " m" 'evil-jump-item)
(define-key evil-normal-state-map ",," 'evil-buffer)
(define-key evil-normal-state-map "-" 'delete-other-windows)
(define-key evil-normal-state-map "B" 'ido-switch-buffer)
(define-key evil-normal-state-map "E" 'ido-find-file)
(define-key evil-normal-state-map "\\" 'evil-repeat-find-char-reverse)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
(define-key evil-normal-state-map "L" 'evil-last-non-blank)
(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
;; ( define-key evil-normal-state-map "B" 'magit-checkout)

(define-key evil-normal-state-map (kbd "C-w C-w") 'other-window)
(define-key evil-normal-state-map (kbd "C-w w") 'other-window)
(define-key evil-normal-state-map (kbd "C-w n") 'split-window-vertically)
(define-key evil-normal-state-map (kbd "C-w C-n") 'split-window-vertically)

;; (define-key evil-normal-state-map (kbd "<C-return>") 'new-line-in-normal-mode)
;; (define-key evil-normal-state-map (kbd "M-t") 'projectile-find-file)
;; (define-key evil-normal-state-map (kbd "M-f") 'dired)
;; (define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
;; (define-key evil-normal-state-map (kbd "M-j") 'evil-window-next)
;; (define-key evil-normal-state-map (kbd "M-.") 'my-find-tag)
;; (define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
;; (define-key evil-normal-state-map (kbd "C-SPC") 'comment-or-uncomment-region-or-line)
;; (define-key evil-normal-state-map (kbd "M-k") 'cycle-buffer)
;; (define-key evil-normal-state-map (kbd "M-K") 'cycle-buffer-backward)
;; (define-key evil-normal-state-map (kbd "M-o") 'session-jump-to-last-change)

;; (define-key evil-normal-state-map (kbd "C-k") 'smart-up)
;; (define-key evil-normal-state-map (kbd "C-j") 'smart-down)
;; (define-key evil-normal-state-map (kbd "C-l") 'smart-forward)
;; (define-key evil-normal-state-map (kbd "C-h") 'smart-backward)
;; (evil-define-key 'visual global-map (kbd ",re") 'dr/extract-variable)
;; (evil-define-key 'normal global-map (kbd ",ri") 'dr/inline-variable)

(defalias 'eon 'turn-on-evil-mode)
(defalias 'eoff 'turn-off-evil-mode)

(provide 'init-evil)
