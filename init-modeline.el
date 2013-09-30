(defun eproject-name-or-user-home ()
  "return eproject root or user home directory"
  (if (ignore-errors (eproject-name))
      (eproject-name)
    ""))

;; Mode line setup
(setq-default
 mode-line-format
 '(

   ;; 코딩시스템
   (:propertize "%z" face mode-line-position-face)
   (:propertize "%Z" face mode-line-position-face)

   ;; 라인과 컬럼
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ;; emacsclient [default -- keep?]
   mode-line-client

   " "
   ;; 읽기전용인지 수정되었는지의 상황을 표시한다
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " changed! " 'face 'mode-line-modified-face))
          (t " ")))
   " "

   ;; 프로젝트 이름을 표시한다
   (:propertize (:eval (eproject-name-or-user-home))
                face mode-line-project-name-face)
   " "

   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n "
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "

   ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-project-name-face)
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)


(set-face-attribute 'mode-line nil
                    :background "white"
                    :box '(:line-width 1 :color "#99CC99")
                    :inherit 'default
                    :height 0.8
                    )

(set-face-attribute 'mode-line-inactive nil
                    :background "#EEE"
                    :box '(:line-width 1 :color "#AAAAAA")
                    :height 0.8
                    )

;; project-name
(set-face-attribute 'mode-line-project-name-face nil
                    :inherit 'mode-line-face
                    :foreground "#009933"
                    :weight 'bold)

;; read-only
(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#FF4E00"))

;; changed
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "#ffffff"
                    :box '(:line-width 2 :color "#c82829"))

;; fold
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    :foreground "gray60")

;; filename
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#FF4E00")

;; line
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face
                    :foreground "#9999CC"
                    :family "Verdana")

;; mode
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "black"
                    )
;; minor
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "gray40")

(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718c00")

(provide 'init-modeline)
