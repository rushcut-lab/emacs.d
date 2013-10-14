(require-package 'paredit)
(setq ruby-deep-indent-paren nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RUBY-MODE DETECT
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-auto-mode 'ruby-mode "Rakefile" "\\.rake\\'" "\\.ru\\'" "\\.prawn\\'"
               "Gemfile\\'" "Capfile\\'" "Guardfile\\'" "\\.gemspec$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RVM
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'rvm)
(rvm-use-default)

(setq current-rvmrc nil)

(defun rvm-corresponding-ruby-eproject ()
  (if (ignore-errors (eproject-name))
      (let ((rvmrc (concat (eproject-name) "/.rvmrc")))
        (if (file-exists-p rvmrc)
            (unless (= current-rvmrc rvmrc)
              (rvm-activate-corresponding-ruby))
          )
        (setq current-rvmrc rvmrc))))

;; use rvm when buffer changed.
;; TODO: DRY
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (rvm-corresponding-ruby-eproject)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (rvm-corresponding-ruby-eproject)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (rvm-corresponding-ruby-eproject)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (rvm-corresponding-ruby-eproject)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (rvm-corresponding-ruby-eproject)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (rvm-corresponding-ruby-eproject)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FEATURE-MODE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'feature-mode)
(setq feature-default-language "ko")

;; Save current buffer (not feature buffer)
(defadvice feature-run-cucumber (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RINARI
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rinari)
(global-rinari-mode)
(setq feuature-use-rvm t)

(eval-after-load 'rinari
  '(progn
     (diminish 'rinari-minor-mode "Rin")
     (define-key rinari-minor-mode-map (kbd "C-c r f") 'rinari-find-features)
     (define-key rinari-minor-mode-map (kbd "C-c r u") 'rinari-find-features-support)
     (define-key rinari-minor-mode-map (kbd "C-c r m") 'rinari-find-model)
     (define-key rinari-minor-mode-map (kbd "C-c r c") 'rinari-find-controller)
     (define-key rinari-minor-mode-map (kbd "C-c r s") 'rinari-find-steps)
     (define-key rinari-minor-mode-map (kbd "C-c r t") 'rinari-find-stylesheet)
     (define-key rinari-minor-mode-map (kbd "C-c r r") 'rinari-find-rspec)
     (define-key rinari-minor-mode-map (kbd "C-c r i") 'rinari-find-rspec-integration)
     (define-key rinari-minor-mode-map (kbd "C-c r v") 'rinari-find-view)
     (define-key rinari-minor-mode-map (kbd "C-c r h") 'rinari-find-helper)
     (define-key rinari-minor-mode-map (kbd "C-c r p") 'rinari-find-file-in-project)
     (define-key rinari-minor-mode-map (kbd "C-c r w") 'rinari-find-lib)
     (define-key rinari-minor-mode-map (kbd "C-c r l") 'rinari-find-locale)
     (define-key rinari-minor-mode-map (kbd "C-c r g") 'rinari-find-configuration)
     (define-key rinari-minor-mode-map (kbd "C-c r o") 'rinari-find-routes)
     (define-key rinari-minor-mode-map (kbd "C-c r y") 'rinari-find-layouts)
     (define-key rinari-minor-mode-map (kbd "C-c r a") 'rinari-find-view-application)
     (define-key rinari-minor-mode-map (kbd "C-c r q") 'rinari-find-gemfile)

     (define-key rinari-minor-mode-map (kbd "C-c r j c") 'rinari-find-ember-controller)
     (define-key rinari-minor-mode-map (kbd "C-c r j m") 'rinari-find-ember-model)
     (define-key rinari-minor-mode-map (kbd "C-c r j v") 'rinari-find-ember-view)
     (define-key rinari-minor-mode-map (kbd "C-c r j t") 'rinari-find-ember-template)
     (define-key rinari-minor-mode-map (kbd "C-c r j o") 'rinari-find-ember-routes)
     (define-key rinari-minor-mode-map (kbd "C-c r j s") 'rinari-find-ember-store)
     (define-key rinari-minor-mode-map (kbd "C-c r j h") 'rinari-find-ember-helper)
     (define-key rinari-minor-mode-map (kbd "C-c r j p") 'rinari-find-ember-all)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RSPEC-MODE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rspec-mode)
(add-hook 'feature-mode-hook 'rvm-activate-corresponding-ruby)

;; (defgroup evil-rails nil
;;   "Evil Rails customizations."
;;   :prefix "evil-rails-"
;;   :group 'evil-rails)

;; (defvar evil-rails-minor-mode-hook nil
;;   "*Hook for customising evil-rails.")

;; (evil-ex-define-cmd "Rfile"       'rinari-find-file-in-project)
;; (evil-ex-define-cmd "Rcontroller" 'rinari-find-controller)
;; (evil-ex-define-cmd "Rmodel"      'rinari-find-model)
;; (evil-ex-define-cmd "Rview"       'rinari-find-view)
;; (evil-ex-define-cmd "Rspec"       'rinari-find-rspec)
;; (evil-ex-define-cmd "Rhelper"     'rinari-find-helper)
;; (evil-ex-define-cmd "Rmailer"     'rinari-find-mailer)
;; (evil-ex-define-cmd "Rmigration"  'rinari-find-migration)
;; (evil-ex-define-cmd "Rstylesheet" 'rinari-find-stylesheet)
;; (evil-ex-define-cmd "Rsass"       'rinari-find-sass)
;; (evil-ex-define-cmd "Rjavascript" 'rinari-find-javascript)
;; (evil-ex-define-cmd "Rfeature"    'rinari-find-festures)
;; (evil-ex-define-cmd "Rserver"     'rinari-web-server-restart)

(define-key ruby-mode-map (kbd "C-c r k") 'ruby-support-create-reverse-file)

(provide 'init-rails)
