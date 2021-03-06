(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]*$" "" str))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))


;;----------------------------------------------------------------------------
;; TODO: CLEAN
;;----------------------------------------------------------------------------

(defun add-auto-mode (mode &rest patterns)
  "Associate every pattern in `PATTERNS' with `MODE'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defmacro add-lambda (hook &rest body)
  "Wrap `BODY' in a lambda, and add it to `HOOK'."
  (declare (indent 1))
  `(add-hook ,hook (lambda () ,@body)))

(defvar webjump-api-sites nil)

(make-variable-buffer-local 'webjump-api-sites)

(defun webjump-api ()
  (interactive)
  (require 'webjump)
  (let* ((completion-ignore-case t)
         (default (caar webjump-api-sites))
         (url (cdr (assoc-string
                    (completing-read "Search API: " webjump-api-sites nil t
                                     nil nil default)
                    webjump-api-sites t)))
         (name (completing-read "Name: " nil nil nil (thing-at-point 'symbol))))
    (browse-url (if (webjump-null-or-blank-string-p name)
                    url
                  (concat url (webjump-url-encode name))))))

(defvar normal-local-function-key-map nil)

(defun rename-file-and-buffer ()
  "Rename the current buffer and the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil) t))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun launch (command)
  "Launch an application from Emacs, with its own output
buffer. This is like asynch-shell-command but allows for any
number of processes at a time, rather than just one. If given a
prefix argument, the process's buffer is displayed."
  (interactive (list (read-shell-command (concat default-directory "$ "))))
  (let* ((name (car (split-string-and-unquote command)))
         (buffer (generate-new-buffer (concat "*" name "*"))))
    (set-process-sentinel (start-process-shell-command name buffer command)
                          'launch-sentinel)
    (if (eq (car current-prefix-arg) 4)
        (display-buffer buffer))))

(defun launch-sentinel (proc event)
  "Reports on changes in `launch'ed applications."
  (message (format "%s: %s" proc event)))

(defun diff-current-buffer-with-file ()
  (interactive)
  "Diff the current buffer with the content saved in the file."
  (diff-buffer-with-file (current-buffer)))

(defun delete-current-file ()
  "Delete the file associated with the current buffer.
Delete the current buffer too.
If no file is associated, just close buffer without prompt for save."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (yes-or-no-p (concat "Delete file?: " currentFile))
      (kill-buffer (current-buffer))
      (when (not (equal currentFile nil))
        (delete-file currentFile)))))

(defun make-backup ()
  "Make a backup copy of current buffer's file.
Create a backup of current buffer's file.
The new file name is the old file name with “~” added at the end, in the same dir.
If such a file already exist, append more “~”.
If the current buffer is not associated with a file, its a error."
  (interactive)
  (let (cfile bfilename)
    (setq cfile (buffer-file-name))
    (setq bfilename (concat cfile "~"))

    (while (file-exists-p bfilename)
      (setq bfilename (concat bfilename "~")))

    (copy-file cfile bfilename t)
    (message (concat "Backup saved as: " (file-name-nondirectory bfilename)))))

(defun open-current-file-as-admin ()
  "Open the current buffer as unix root.
This command works on unixes only."
  (interactive)
  (when buffer-file-name (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun change-file-newline (fpath eol-system)
  "Change file's line ending to unix convention.
FPATH is full path to file.
eol-system is one of “'unix”, “'dos”, “'mac”.
The “'dos” means Windows's convention.
The “'mac” means Mac OS Classic's convention.
For Mac OS X, use “'unix”."
  (let (mybuffer)
    (setq mybuffer (find-file fpath))
    (set-buffer-file-coding-system eol-system)
    (save-buffer)
    (kill-buffer mybuffer)))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

(defun how-many-in-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
 Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "norwegian") "english" "norwegian")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(defun revert-all-buffers ()
  "Revert all non-modified buffers associated with a file.
This is to update existing buffers after a Git pull of their underlying files."
  (interactive)
  (save-current-buffer
    (mapc (lambda (b)
            (set-buffer b)
            (unless (or (null (buffer-file-name)) (buffer-modified-p))
              (revert-buffer t t)
              (message "Reverted %s\n" (buffer-file-name))))
          (buffer-list))))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(defun string/ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun open-all-files-with-extension (dir extension)
  "Open all files below dir with the given extension."
  (interactive "DBase directory: \nsExtension: ")
  (let* ((list (directory-files dir t "^[^.]"))
         (files (remove-if 'file-directory-p list))
         (dirs (remove-if-not 'file-directory-p list)))
    (dolist (file files)
      (if (string/ends-with file extension)
          (find-file-noselect file)))
    (dolist (dir dirs)
      (find-file-noselect dir)
      (open-all-files-with-extension dir extension))))

;; (defun* get-closest-pathname (&optional (file "Makefile"))
;;   "Determine the pathname of the first instance of FILE starting from the
;;  current directory towards root. This may not do the correct thing in presence
;;  of links. If it does not find FILE, then it shall return the name of FILE in
;;  the current directory, suitable for creation"
;;   (let ((root (expand-file-name "/")))
;;     (expand-file-name file
;;                       (loop
;;                        for d = default-directory then (expand-file-name ".." d)
;;                        if (file-exists-p (expand-file-name file d))
;;                        return d
;;                        if (equal d root)
;;                        return nil))))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position)))))))
  (evil-scroll-line-to-top nil))

(defun mediawiki-insert-sub-header ()
  "Insert subheader  via  === (e.g. === FOO ===.)"
  (interactive)
  (mediawiki-insert "===" "==="))

(defun ergoemacs-fix-cua--pre-command-handler-1 ()
  "Fixes CUA minor mode so selection is highlighted only when
Shift+<special key> is used (arrows keys, home, end, pgdn, pgup, etc.)."
  (defun cua--pre-command-handler-1 ()
    ;; Cancel prefix key timeout if user enters another key.
    (when cua--prefix-override-timer
      (if (timerp cua--prefix-override-timer)
          (cancel-timer cua--prefix-override-timer))
      (setq cua--prefix-override-timer nil))

    (cond
     ;; Only symbol commands can have necessary properties
     ((not (symbolp this-command))
      nil)

     ;; Handle delete-selection property on non-movement commands
     ((not (eq (get this-command 'CUA) 'move))
      (when (and mark-active (not deactivate-mark))
        (let* ((ds (or (get this-command 'delete-selection)
                       (get this-command 'pending-delete)))
               (nc (cond
                    ((not ds) nil)
                    ((eq ds 'yank)
                     'cua-paste)
                    ((eq ds 'kill)
                     (if cua--rectangle
                         'cua-copy-rectangle
                       'cua-copy-region))
                    ((eq ds 'supersede)
                     (if cua--rectangle
                         'cua-delete-rectangle
                       'cua-delete-region))
                    (t
                     (if cua--rectangle
                         'cua-delete-rectangle ;; replace?
                       'cua-replace-region)))))
          (if nc
              (setq this-original-command this-command
                    this-command nc)))))

     ;; Handle shifted cursor keys and other movement commands.
     ;; If region is not active, region is activated if key is shifted.
     ;; If region is active, region is cancelled if key is unshifted
     ;;   (and region not started with C-SPC).
     ;; If rectangle is active, expand rectangle in specified direction and
     ;;   ignore the movement.
     ((if window-system
          ;; Shortcut for window-system, assuming that input-decode-map is empty.

          ;; ErgoEmacs patch begin ------------------
        ;;;; (memq 'shift (event-modifiers
        ;;;;               (aref (this-single-command-raw-keys) 0)))
          (and (memq 'shift (event-modifiers
                             (aref (this-single-command-raw-keys) 0)))
               ;; In this way, we expect to use CUA only with keys that
               ;; are symbols (like <left>, <next>, etc.)
               (symbolp (event-basic-type (aref (this-single-command-raw-keys) 0))))
        ;; ErgoEmacs patch end --------------------

        (or
         ;; Check if the final key-sequence was shifted.
         (memq 'shift (event-modifiers
                       (aref (this-single-command-keys) 0)))
         ;; If not, maybe the raw key-sequence was mapped by input-decode-map
         ;; to a shifted key (and then mapped down to its unshifted form).
         (let* ((keys (this-single-command-raw-keys))
                (ev (lookup-key input-decode-map keys)))
           (or (and (vector ev) (memq 'shift (event-modifiers (aref ev 0))))
               ;; Or maybe, the raw key-sequence was not an escape sequence
               ;; and was shifted (and then mapped down to its unshifted form).
               (memq 'shift (event-modifiers (aref keys 0)))))))
      (unless mark-active
        (push-mark-command nil t))
      (setq cua--last-region-shifted t)
      (setq cua--explicit-region-start nil))

     ;; Set mark if user explicitly said to do so
     ((or cua--explicit-region-start cua--rectangle)
      (unless mark-active
        (push-mark-command nil nil)))

     ;; Else clear mark after this command.
     (t
      ;; If we set mark-active to nil here, the region highlight will not be
      ;; removed by the direct_output_ commands.
      (setq deactivate-mark t)))

    ;; Detect extension of rectangles by mouse or other movement
    (setq cua--buffer-and-point-before-command
          (if cua--rectangle (cons (current-buffer) (point))))))

(defun kill-region-or-backward-kill-word ()
  "Call kill region, if region is active, otherwise backward-kill-word"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (backward-kill-word 1)))

(defun give-my-keybindings-priority ()
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'ex-mode))
      (let ((mykeys (assq 'ex-mode minor-mode-map-alist))
            (override-keys-fn
             (intern (concat (symbol-name major-mode) "-override"))))
        (assq-delete-all 'ex-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys)
        (if (functionp override-keys-fn)
            (funcall override-keys-fn)))))

(defun fill-keymap (keymap &rest mappings)
  "Fill `KEYMAP' with `MAPPINGS'.
See `pour-mappings-to'."
  (pour-mappings-to keymap mappings))

(defun fill-keymaps (keymaps &rest mappings)
  "Fill `KEYMAPS' with `MAPPINGS'.
See `pour-mappings-to'."
  (dolist (keymap keymaps keymaps)
    (let ((map (if (symbolp keymap)
                   (symbol-value keymap)
                 keymap)))
      (pour-mappings-to map mappings))))

(defun cofi/set-key (map spec cmd)
  "Set in `map' `spec' to `cmd'.

`Map' may be `'global' `'local' or a keymap.
A `spec' can be a `read-kbd-macro'-readable string or a vector."
  (let ((setter-fun (case map
                      (global #'global-set-key)
                      (local #'local-set-key)
                      (t (lambda (key def) (define-key map key def)))))
        (key (typecase spec
               (vector spec)
               (string (read-kbd-macro spec))
               (t (error "wrong argument")))))
    (funcall setter-fun key cmd)))

(defun take (n lst)
  "Return atmost the first `N' items of `LST'."
  (let (acc '())
    (while (and lst (> n 0))
      (decf n)
      (push (car lst) acc)
      (setq lst (cdr lst)))
    (nreverse acc)))

(defun group (lst n)
  "Group `LST' into portions of `N'."
  (let (groups)
    (while lst
      (push (take n lst) groups)
      (setq lst (nthcdr n lst)))
    (nreverse groups)))

(defun pour-mappings-to (map mappings)
  "Calls `cofi/set-key' with `map' on every key-fun pair in `MAPPINGS'.
`MAPPINGS' is a list of string-fun pairs, with a `READ-KBD-MACRO'-readable string and a interactive-fun."
  (dolist (mapping (group mappings 2))
    (cofi/set-key map (car mapping) (cadr mapping)))
  map)

(defun open-line-below ()
  "Insert a line below the current line without moving point."
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))

(defun open-line-above ()
  "Insert a line above the current line without moving point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (newline)))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented region"))
      (progn
        (indent-buffer)
        (message "Indented buffer")))))

(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.
 (This command always run the saved version.)

If the file is emacs lisp, run the byte compiled version if appropriate."
  (interactive)
  (let (suffixMap fName fSuffix progName cmdStr)

    ;; a keyed list of file suffix to comand-line program path/name
    (setq suffixMap
          '(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("js" . "js")
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")))

    (setq fName (buffer-file-name))
    (setq fSuffix (file-name-extension fName))
    (setq progName (cdr (assoc fSuffix suffixMap)))
    (setq cmdStr (concat progName " \""   fName "\""))

    (when (buffer-modified-p)
      (progn
        (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer))))

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (progn
          (load (file-name-sans-extension fName)))
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*run-current-file output*" ))
        (message "No recognized program file suffix for this file.")))))

(defun load-if-exists (file)
  "Calls LOAD on FILE if FILE exists."
  (let ((file-exists (file-exists-p (expand-file-name file))))
    (when file-exists
      (load (expand-file-name file)))
    file-exists))

(defun directory-dirs (dir)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file
                                     (directory-dirs file))
                               dirs))))))
    dirs))

(defun load-from-vendor-dir ()
  "Loads everything below the vendor folder in `user-emacs-directory'."
  (unless (eq system-type 'windows-nt)
    (let* ((vendor-dir (concat user-emacs-directory "vendor"))
           (files (split-string
                   (shell-command-to-string
                    (concat "find " vendor-dir " -type f -iname '*.el'")))))
      (mapc #'load files))))

(defun load-elisp-files-in-dir (dir &optional regexp count)
  "Load all elisp files in in DIR.  When REGEXP is provided match
only those files with name of form REGEXP.el.  If COUNT is
provided only load COUNT number of such files.  "
  (let* ((regexp (if regexp
                     regexp
                   ".*"))
         (files (directory-files dir t (concat regexp "\.el\$")))
         (count (if count count (length files))))
    (loop for file in files
          for file-num to count
          while (<= file-num count)
          do (load file))))

(defmacro defkeymap (symbol &rest mappings)
  "Define keymap bound to `symbol'.
See `pour-mappings-to'"
  `(progn (unless (boundp ',symbol)
            (defvar ,symbol (make-sparse-keymap)))
          (fill-keymap ,symbol ,@mappings)))

(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(defun toggle-bury-compilation-buffer ()
  (interactive)
  (if compilation-finish-function
      (progn
        (setq compilation-finish-function nil)
        (message "Bury buffer: OFF."))
    (setq compilation-finish-function
          (lambda (buf str)
            (if (string-match "exited abnormally" str)
                (message "Compilation errors, press M-n to visit")

              (run-at-time 1.3 nil #'switch-to-prev-buffer
                           (get-buffer-window buf) 'append)

              (message "Compilation succesful!"))))
    (message "Bury buffer: ON.")))

(defun chop (string)
  "Returns a new string with the last char removed."
  (subseq string 0 -1))

(defun chomp (string)
  "Returns a new string removing zero or one newline from `string'."
  (if (string/ends-with string "\n")
      (chop string)
    string))

(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun path-to-executable (program)
  "Returns the path to `program' or NIL.  Relies on which, so
only works on *nix."
  (let* ((which (chomp (shell-command-to-string (concat "which " program))))
         (executable (if (string-match "not found" which)
                         nil
                       which)))
    executable))

(defun inside-string-p ()
  "T if point is inside a string, NIL otherwise."
  (nth 3 (syntax-ppss)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun debug-config (test-fn)
  "Repeatedly calls `test-fn' to verify that everything is OK
while loading init files.  `test-fn' should return NIL to
indicate failure."
  (when test-fn
    (find-file "/tmp/debug-config")
    (loop for file in (directory-files user-emacs-directory t "^init-.*\.el\$")
          always (funcall test-fn)
          do (progn
               (set-buffer "debug-config")
               (insert (format "Test OK.  Loading file %s..." file))
               (save-buffer)
               (load file))
          finally (delete-file "/tmp/debug-config"))))

(defun edit-user-config-file ()
  "Use completing read to open one of the configuration files for
  emacs for editing."
  (interactive)
  (find-file (ido-read-file-name "Edit file: " user-emacs-directory nil
                                 nil "init-")))

(defun change-whitespace-line-column (c)
  (interactive "nColumn: ")
  (make-local-variable 'whitespace-line-column)
  (setq whitespace-line-column c)
  (message (format "Highlighting lines longer than %s columns." c)))


(provide 'init-utils)
