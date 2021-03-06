(defvar user-home-directory (concat (getenv "HOME") "/"))

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(add-to-list 'load-path user-emacs-directory)
(require 'init-benchmarking) ;; Measure startup time

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(defconst *spell-check-support-enabled* nil)
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
;; (require 'init-exec-path) ;; Set up $PATH

(when *is-a-mac*
  (require-package 'osx-location))

;;----------------------------------------------------------------------------
;;
;; CORE
;;
;;----------------------------------------------------------------------------
(require 's)
(require 'f)
(require 'init-emacs-preference)
(require 'init-editing)
(require 'init-completion)
(require 'init-project)
(require 'init-font)
(require 'init-ido)
(require 'init-textmate)
(require 'init-git)
(require 'init-ack)
(require 'init-cursor-move)
(require 'init-tags)
(require 'init-my-functions)
(require 'init-yasnippet)
(require 'init-auto-save)
(require 'init-parens)
(require 'init-regexp)

;;----------------------------------------------------------------------------
;;
;; THEMES
;;
;;----------------------------------------------------------------------------
(require 'init-theme)
(require 'init-modeline)

;;----------------------------------------------------------------------------
;;
;; ADV
;;
;;----------------------------------------------------------------------------
;; (require 'init-evil)



;;----------------------------------------------------------------------------
;;
;; MODES
;;
;;----------------------------------------------------------------------------
(require 'init-rails)
(require 'init-lisp)
(require 'init-org)



;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

(message "init completed in %.2fms"
         (sanityinc/time-subtract-millis (current-time) before-init-time))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:


(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -c 'import sys,json; data=json.loads(sys.stdin.read()); print json.dumps(data,sort_keys=True,indent=4).decode(\"unicode_escape\").encode(\"utf8\",\"replace\")'" (current-buffer) t)))


(defun ruby-run-this-buffer ()
  (interactive)
  (save-buffer)
  (setq cur (selected-window))
  (ruby-compilation-this-buffer)
  (select-window cur))

(define-key ruby-mode-map (kbd "C-x C-e") 'ruby-run-this-buffer)
