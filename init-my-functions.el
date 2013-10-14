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

;;----------------------------------------------------------------------------
;; Ruby Snippets Supports
;;----------------------------------------------------------------------------

(defun last-string (text)
  (car (last (split-string text "\\."))))

(defun singularize-last-string (text)
  (singularize-string (last-string text)))

(defun ruby-last-value (text)
  "@를 제거하고 .으로 연결된 값 중 마지막 값을 리턴한다"
  (replace-regexp-in-string "@" "" (car (last (split-string text "\\.")))))

(defun ruby-singularize-last-string (text)
  "ruby-last-value의 단수형을 리턴한다"
  (singularize-string (ruby-last-value text)))

(defun ruby-relative-source-path-dir ()
  "spec이나 lib 이후의 경로를 리턴한다"
  (let ((buffer-dir (file-name-directory buffer-file-name))
        (strip-slash-regexp "\\(^/\\|/$\\)"))
    (replace-regexp-in-string
     strip-slash-regexp ""
     (replace-regexp-in-string ".*\\(lib\\|spec\/views\\|spec\/integration\\|spec\\)" "" buffer-dir))
    ))

(defun ruby-relative-source-path-dir-as-list ()
  "spec이나 lib 이후의 경로를 리스트로 리턴한다"
  (split-string (ruby-relative-source-path-dir) "/"))

(defun camelize (str)
  (mapconcat (lambda (x) (capitalize x)) (split-string str "_") ""))

(defun ruby-module-defs-ends ()
  (let ((list (ruby-relative-source-path-dir-as-list)))
    (if (= (length (car list)) 0)
        nil
      (mapconcat 'identity (mapcar (lambda (x) (concat "end")) list) "\n"))))

(defun ruby-module-defs ()
  (let ((list (ruby-relative-source-path-dir-as-list)))
    (if (= (length (car list)) 0)
        ""
      (mapconcat 'identity (mapcar (lambda (x) (concat "module " (camelize x) "")) list) "\n"))))

(defun cheeso-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    ;; split <foo><bar> or </foo><bar>, but not <foo></foo>
    (goto-char begin)
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (incf end))
    ;; put xml namespace decls on newline
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n") (incf end))
    (indent-region begin end nil)
    (normal-mode))
  (message "All indented!"))

(defun replace-in-string (string regexp newtext &optional literal)
  (let ((start 0) tail)
    (while (string-match regexp string start)
      (setq tail (- (length string) (match-end 0)))
      (setq string (replace-match newtext nil literal
                                  string))
      (setq start (1- (- (length string) tail)))))
  string)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun focus-iterm ()
  (interactive)
  (shell-command "osascript -e 'tell application \"iTerm\" to activate'"))

;; 2013-07-10
(defun file-name-substr (text &optional prefix &optional postfix)
  (if prefix
      (setq text (replace-regexp-in-string (concat ".*" prefix "?") "" text)))
  (if postfix
      (setq text (replace-regexp-in-string (concat postfix "$") "" text)))
  text
  )
(defun file-name-substr-extensions (text &optional prefix)
  (if prefix
      (setq text (replace-regexp-in-string (concat ".*" prefix "?") "" text)))
  (setq text (replace-regexp-in-string "\\..*$" "" text))
  text)

(defun ruby-spec-name-to-src-name (spec-name)
  "spec이름을 소스 이름으로 반환한다"
  (replace-in-string spec-name "_spec.rb" ".rb"))

(defun ruby-main-dir (path)
  "spec/ 이후에 의미있는 디렉토리 경로를 리턴한다"
  (replace-in-string path (concat (eproject-name) "/spec") ""))

(defun ruby-spec-path-to-src-path (spec-path)
  "spec경로를 소스 경로로 반환한다"
  (let ((src-name (ruby-spec-name-to-src-name (file-name-base spec-path)))
        (main-path (ruby-main-dir spec-path))
        (result nil))
    (if (string-match-p "models\\||controllers\\||routes\\||helpers\\||views" (ruby-main-dir spec-path))
        (setq result (f-join "app" main-path)))
    (if (string-match-p "^lib")
        (setq result main-path)
      (setq result (f-join "lib" main-path)))
    result))

(defun rspec-first-desc ()
  (interactive)
  (setq describe-line
        (if (string-match "spec/views" buffer-file-name)
            (concat "describe '" (file-name-substr-extensions (buffer-file-name) "spec/views/") "' do")
          (if (string-match "integration_spec.rb" buffer-file-name)
              (concat "describe \"" (camelize (file-name-base (replace-regexp-in-string "_integration_spec\\..*$" "" (buffer-name)))) "\", \"integrated\" do")
            (concat "describe " (camelize (file-name-base (replace-regexp-in-string "_spec\\..*$" "" (buffer-name)))) " do"))))

  (if (rails-spec-file-p)
      (concat describe-line "\n" "\n" "end\n")
    (concat (if (ruby-module-defs) (concat (ruby-module-defs) "\n"))
            (concat describe-line "\n\nend")
            "\n"
            (ruby-module-defs-ends)
            )
    ))

(defun rails-spec-file-p ()
  (interactive)
  (string-match "spec/controllers\\|views\\|routes\\|models\\|helpers" buffer-file-name))


(defvar *ruby-support-use-spec-prefix-as-lib* nil)

(defun ruby-support-reverse-file-path (file-path)
  (if (string-match-p "/spec/" file-path)
      (ruby-support-src-path file-path)
    (ruby-support-spec-path file-path)
    ))

(defun ruby-support-create-reverse-file ()
  (interactive)
  (let ((target (f-join eproject-root (ruby-support-reverse-file-path (buffer-file-name)))))
    (shell-command (concat "mkdir -p " (f-dirname target)))
    (shell-command (concat "touch " target))
    ))
;;

(defun ruby-support-delete-spec-poxfix (spec-file-path)
  "파일경로에서 마직막 _spec.rb를 .rb로 변경하여 리턴한다"
  (replace-in-string spec-file-path "_spec.rb" ".rb"))

(defun ruby-support-attach-spec-poxfix (path)
  "파일경로에서 마직막 .rb를 _spec.rb로 변경하여 리턴한다"
  (if (string-match-p ".rb" path)
      (replace-in-string path ".rb" "_spec.rb")
    (concat path "_spec.rb")
    ))

(defun ruby-support-lib-file-p (spec-file-path)
  (not
   (string-match-p
    "/\\(models\\|views\\|controllers\\|integration\\|routing\\|helpers\\)/"
    spec-file-path)))

(defun ruby-support-src-path (spec-file-path)
  "spec 파일의 경로가 주어지면 소스파일의 경로가 리턴되어야 한다"
  (let ((src-path-regardless-prefix (ruby-support-spec-meaningful-path
                                     (ruby-support-delete-spec-poxfix spec-file-path))))
    (if (ruby-support-lib-file-p spec-file-path)
        (if (string-match-p "lib/" spec-file-path)
            src-path-regardless-prefix
          (f-join "lib" src-path-regardless-prefix))
      (f-join "app" src-path-regardless-prefix))))

(defun ruby-support-spec-path (src-file-path)
  "소스파일을 스펙파일 경로로 변환하여 리턴한다"
  (let ((path (ruby-support-attach-spec-poxfix
                      (ruby-support-src-meaningful-path src-file-path))))
    (if (ruby-support-lib-file-p path)
        (if *ruby-support-use-spec-prefix-as-lib*
            (f-join "spec" path)
          (f-join "spec" (replace-in-string path "lib/" "")))
      (f-join "spec" (replace-in-string path "app/" ""))
      )))

(defun ruby-support-src-meaningful-path (src-file-path)
  "소스 파일의 경로가 주어지면 루트 이후의 경로를 리턴해야 한다"
  (replace-in-string src-file-path ".*?\\(app\\|lib\\)" "\\1"))

(defun ruby-support-spec-meaningful-path (spec-file-path)
  "spec 파일의 경로가 주어지면 spec/ 이후의 경로를 리턴해야 한다"
  (replace-in-string spec-file-path ".*/spec/" ""))

(defun ruby-support-method-move-down ()
  (interactive)
  (let ((pattern "\s*def\s")
        (line (thing-at-point 'line)))
    (if (string-match pattern line) (next-line))
    (unless (search-forward-regexp pattern nil t)
      (beginning-of-buffer)
      ))
  (recenter))

(defun ruby-support-method-move-up ()
  (interactive)
  (let ((pattern "\s*def\s")
        (line (thing-at-point 'line)))
    (if (string-match pattern line) (previous-line))
    (unless (search-backward-regexp pattern nil t)
      (end-of-buffer)
      ))
  (recenter))

(global-set-key (kbd "C-M-n") 'ruby-support-method-move-down)
(global-set-key (kbd "C-M-p") 'ruby-support-method-move-up)


(provide 'init-my-functions)
