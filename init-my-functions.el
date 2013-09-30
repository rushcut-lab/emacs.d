;;----------------------------------------------------------------------------
;; Kill All Buffers
;;----------------------------------------------------------------------------
(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;----------------------------------------------------------------------------
;; Background Iterm
;;----------------------------------------------------------------------------
(defun focus-iterm ()
  (interactive)
  (shell-command "osascript -e 'tell application \"iTerm\" to activate'"))

(if window-system
    (progn (global-unset-key (kbd "C-z"))
           (global-unset-key (kbd "C-x C-z"))
           (global-set-key (kbd "C-z") 'focus-iterm)))

;;----------------------------------------------------------------------------
;; Opener
;;----------------------------------------------------------------------------
(defun open-finder-current-buffer ()
  "현재 버퍼를 파인더에서 연다"
  (interactive)
  (shell-command (concat "open " (file-name-directory buffer-file-name)))
  )

(defun open-textmate-current-buffer ()
  "현재 버퍼를 파인더에서 연다"
  (interactive)
  (shell-command (concat "mate " (file-name-directory buffer-file-name)))
  )

(defun open-iterm-current-buffer ()
  "현재 버퍼를 ITerm에서 연다"
  (interactive)
  (shell-command (concat "open -a 'ITerm' " (file-name-directory buffer-file-name)))
  )

(defun open-firefox-current-buffer ()
  "현재 버퍼를 ITerm에서 연다"
  (interactive)
  (shell-command (concat "open -a 'Firefox' " (buffer-file-name)))
  )
(defun open-firefox-current-dir ()
  "현재 버퍼를 ITerm에서 연다"
  (interactive)
  (shell-command (concat "open -a 'Firefox' " (file-name-directory buffer-file-name)))
  )

(provide 'init-my-functions)
