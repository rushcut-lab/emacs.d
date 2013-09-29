(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'zenburn t)

;;(require-package 'color-theme-sanityinc-solarized)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Automatic color cycling to help reduce eyestrain.
;; ;; Copyright 1996-2007 Paton J. Lewis <pjl@symbolcraft.com>
;; ;; Version 1.0

;; (setq cycle-colors-state t)
;; (setq cycle-colors-index 0)
;; (defun cycle-colors nil (interactive)
;;   (cond
;;     ((= cycle-colors-index 0)
;;      (load-theme 'zenburn t)
;;      )
;;     ((= cycle-colors-index 1)
;;      (load-theme 'sanityinc-solarized-light t)
;;      )
;;     ((= cycle-colors-index 2)
;;       (progn (set-foreground-color "purple")
;;              (set-background-color "wheat")
;;              (set-cursor-color     "red"))
;;       )
;;     ((= cycle-colors-index 3)
;;       (progn (set-foreground-color "black")
;;              (set-background-color "white")
;;              (set-cursor-color     "yellow"))
;;       )
;;     ((= cycle-colors-index 4)
;;       (progn (set-foreground-color "blue")
;;              (set-background-color "yellow2")
;;              (set-cursor-color     "gray22")
;; )
;;       ))
;;   (setq cycle-colors-index (1+ cycle-colors-index))
;;   (if (> cycle-colors-index 4) (setq cycle-colors-index 0)))

;; (defun cycle-colors-toggle (state)
;;   (setq cycle-colors-state (if (null state) (not cycle-colors-state) state))
;;   (if cycle-colors-state
;;     (progn (run-with-timer 300 300 'cycle-colors)
;;            (print "Color cycling disabled."))
;;     (progn (cancel-function-timers 'cycle-colors)
;;            (print "Color cycling enabled."))))
;; (cycle-colors-toggle t)

;; (global-set-key "\C-xc" 'cycle-colors) 
;; (global-set-key "\C-xC" '(lambda nil (interactive) (cycle-colors-toggle (not cycle-colors-state))))

(provide 'init-theme)
