(require-package 'diminish)

;;----------------------------------------------------------------------------
;; Diabled
;;----------------------------------------------------------------------------
(global-unset-key (kbd "C-t")) ;; transpose-chars

(transient-mark-mode t)
(global-set-key (kbd "RET") 'newline-and-indent)

;;----------------------------------------------------------------------------
;; Comment
;;----------------------------------------------------------------------------
(global-unset-key (kbd "M-/"))
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

;;----------------------------------------------------------------------------
;; Just one space
;;----------------------------------------------------------------------------
(global-unset-key (kbd "C-c C-SPC"))
(global-set-key (kbd "C-c C-SPC") 'just-one-space)

;;----------------------------------------------------------------------------
;; Macro
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-c m") 'call-last-kbd-macro)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(require-package 'mic-paren)
(paren-activate)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(global-set-key (kbd "M-l") 'er/expand-region)

;;----------------------------------------------------------------------------
;; Auto Revert
;;----------------------------------------------------------------------------
(global-auto-revert-mode)

;;----------------------------------------------------------------------------
;; Undo Tree
;;----------------------------------------------------------------------------
(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;;----------------------------------------------------------------------------
;; Cua Mode
;;----------------------------------------------------------------------------
(cua-selection-mode t)
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "C-\\") 'cua-set-rectangle-mark)

;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
;; (require-package 'whole-line-or-region)
;; (whole-line-or-region-mode t)
;; (diminish 'whole-line-or-region-mode)
;; (make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)

;;----------------------------------------------------------------------------
;; Duplicate Line or Region
;;----------------------------------------------------------------------------
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))
                  ))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-unset-key (kbd "C-c d"))
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)

;;----------------------------------------------------------------------------
;; Mac Clipboard Share Fix
;;----------------------------------------------------------------------------

(defun mac-copy ()
  (shell-command-to-string "pbpaste"))

(defun mac-paste (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'mac-paste)
(setq interprogram-paste-function 'mac-copy)

;;----------------------------------------------------------------------------
;; Move Text
;;----------------------------------------------------------------------------
(require-package 'move-text)
(move-text-default-bindings)
(global-unset-key (kbd "M-p"))
(global-unset-key (kbd "M-n"))
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;----------------------------------------------------------------------------
;; Browse Kill Ring
;;----------------------------------------------------------------------------
(require-package 'browse-kill-ring)
(global-unset-key (kbd "C-M-y"))
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

;;----------------------------------------------------------------------------
;; Whitespace
;;----------------------------------------------------------------------------
(setq whitespace-style '(face tabs trailing lines-tail empty
                              space-before-tab tab-mark))
(require-package 'ethan-wspace)

(eval-after-load "ethan-wspace"
  '(progn
     (define-minor-mode ethan-wspace-highlight-tabs-mode
       :init-value nil :lighter nil :keymap nil)

     (define-minor-mode ethan-wspace-highlight-eol-mode
       :init-value nil :lighter nil :keymap nil)))

(global-ethan-wspace-mode 1)
(setq ethan-wspace-face-customized t)

(defvar whitespace-show-all-mode nil)

(defun* toggle-whitespace-mode ()
  (interactive)
  (when current-prefix-arg
    (if whitespace-mode
        (progn
          (whitespace-mode 0)
          (message "Whitespace mode off"))
      (whitespace-mode 1)
      (message "Whitespace mode on"))
    (return-from toggle-whitespace-mode))
  (if whitespace-show-all-mode
      (progn
        (setq whitespace-style '(face tabs trailing lines-tail empty
                                      space-before-tab tab-mark))
        (setq whitespace-show-all-mode nil)
        (whitespace-mode 0)
        (whitespace-mode 1)
        (message "Highlighting some whitespace"))
    (setq whitespace-style
          '(face tabs spaces trailing lines-tail space-before-tab newline
                 indentation empty space-after-tab space-mark tab-mark
                 newline-mark))
    (setq whitespace-show-all-mode t)
    (whitespace-mode 0)
    (whitespace-mode 1)
    (message "Highlighting all whitespace")))

(defun ethan-wspace-clean-all ()
  "Clean all whitespace errors immediately."
  (interactive)
  (dolist (type ethan-wspace-errors)
    (ethan-wspace-type-clean type))
  (indent-buffer))


(provide 'init-editing)
