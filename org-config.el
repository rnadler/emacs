;; Org mode
(defconst org-mode-directory "~/.emacs.d/org-mode")
(defconst my-org-directory
  (if (string= (system-name) "WIN10R90H8MKJ")
      "//brutus/it/healthinformatics/BobN/org"
      "~/org"))
(defconst todo-org-file (concat my-org-directory "/todo.org"))
(defconst journal-org-file (concat my-org-directory "/journal.org"))
(defconst transfer-org-file "/media/sf_healthinformatics/BobN/org/transfer.org")
(if (file-directory-p org-mode-directory)
    (progn
      (setq load-path (cons (concat org-mode-directory "/lisp") load-path))
      (setq load-path (cons (concat org-mode-directory "/contrib/lisp") load-path))
      (require 'org-install))
    (require 'org))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file todo-org-file)))
(setq org-log-done t)
(setq org-hide-leading-stars nil)
(setq org-startup-indented t)
(unless (eq system-type 'windows-nt)
  (add-hook 'org-mode-hook 'turn-on-flyspell 'append))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline todo-org-file "Tasks")
	 "* TODO %?\n  %i\n  %a\n%T")
	("x" "Transfer" entry (file+headline transfer-org-file "Tasks")
          "* TODO %?\n  %i\n  %a\n%T")
        ("j" "Journal" entry (file+olp+datetree journal-org-file)
	 "* %?\nEntered on %U\n  %i\n  %a")))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (ruby . t)
   (emacs-lisp . t)))
(setq org-agenda-include-diary t)
(setq org-confirm-babel-evaluate nil)
(fset 'do-org-backup
   [?\C-u ?\C-x ?s ?\C-c ?o ?\M-< ?\C-r ?\C-s ?: ?  ?b ?a ?c ?k ?u ?p return ?\C-n ?\C-n ?\C-c ?\C-c])
(global-set-key (kbd "C-c b")
   (lambda ()
     (interactive)
     (let (
         (oldp (point))
         (oldbuff (current-buffer)))
       (execute-kbd-macro (symbol-function 'do-org-backup))
       (unless (eq (current-buffer) oldbuff) (switch-to-buffer oldbuff))
       (goto-char oldp))
     (message "Backup complete.")))
(fset 'my-agenda
   (lambda (&optional arg) "Startup my custom agenda." (interactive "p") (kmacro-exec-ring-item (quote ("ap" 0 "%d")) arg)))
(use-package htmlize
    :defer t)
;; Custom agenda
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-files (list
			todo-org-file
			"~/Projects/emacs/shared.org"
			transfer-org-file))
(when (string= (system-name) "bob-x1-carbon")
    (setq org-agenda-files (delete transfer-org-file org-agenda-files)))
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-custom-commands
      '(("p" "Agenda and Projects"
         ((agenda "" nil)
	  (tags-todo "-TODO=\"DONE\"-TODO=\"CANCELLED\""
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
		 (org-agenda-overriding-header "Unscheduled TODO entries:")
		 (org-agenda-view-columns-initially t)
		 (org-agenda-sorting-strategy '(tag-up alpha-up))))
          (tags "project"
              ((org-agenda-overriding-header "Projects:")
               (org-tags-match-list-sublevels 'indented)))))
	))
;; Nice looking bullets for headings
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(setq org-ellipsis "⤵")
(add-hook 'org-mode-hook 'my/disable-line-numbers)
(add-hook 'org-agenda-finalize-hook 'my/disable-line-numbers)
;; Ivy fix
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
;; Other stuff
(setq org-catch-invisible-edits 'show-and-error)
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
(add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("[ ]" .  "☐") prettify-symbols-alist)
   (push '("[X]" . "☑" ) prettify-symbols-alist)
   (push '("[-]" . "❍" ) prettify-symbols-alist)
   (prettify-symbols-mode)))
(defface org-checkbox-done-text
  '((t (:foreground "#71696A")))
  "Face for the text part of a checked org-mode checkbox.")
(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
    1 'org-checkbox-done-text prepend))
 'append)
