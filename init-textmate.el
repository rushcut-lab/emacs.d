(require-package 'textmate)

(textmate-mode)
;; (diminish 'textmate-mode)

(setq *textmate-gf-exclude*
  "(/|^)(\\.+[^/]+|vendor|fixtures|tmp|log|classes|build|target)($|/)|(\\.xcodeproj|TAGS|\\.class|\\#|\\.nib|\\.framework|\\.app|\\.pbproj|\\.pbxproj|\\.xcode|\\.xcodeproj|\\.bundle|\\.pyc)(/|$)")

;; (defvar *textmate-project-roots*
;;   '(".git" ".hg" "Rakefile" "Makefile" "README" "build.xml" ".emacs-project")
;;   "The presence of any file/directory in this list indicates a project root.")

(setq textmate-use-file-cache nil)

(global-unset-key (kbd "C-M-t"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-t"))

(global-set-key (kbd "M-t") 'textmate-goto-file)
(global-set-key (kbd "C-t") 'textmate-goto-symbol)
(global-set-key (kbd "C-M-t") 'textmate-goto-symbol)

(global-set-key (kbd "M-[") 'textmate-shift-left)
(global-set-key (kbd "M-]") 'textmate-shift-right)

(after-load 'evil
  '(progn
     (define-key evil-normal-state-map ",tc" 'textmate-clear-cache)
     ))


(provide 'init-textmate)
