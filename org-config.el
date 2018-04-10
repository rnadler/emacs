;; Org mode
(defconst org-mode-directory "~/org-mode")
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
        ("j" "Journal" entry (file+datetree journal-org-file)
	 "* %?\nEntered on %U\n  %i\n  %a")))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (ruby . t)
   (emacs-lisp . t)))
(setq org-agenda-include-diary t)
(setq org-confirm-babel-evaluate nil)
(fset 'do-org-backup
      "\C-co\274\C-s: backup\C-n\C-n\C-a\C-c\C-c")
(define-key global-map "\C-cb" 'do-org-backup)
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
(defun my/org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'regexp ">"))
(setq org-agenda-custom-commands
      '(("p" "Agenda and Projects"
         ((agenda "" nil)
	  (tags-todo "-TODO=\"DONE\"-TODO=\"CANCELLED\""
		((org-agenda-skip-function 'my/org-agenda-skip-scheduled)
		 (org-agenda-overriding-header "Unscheduled TODO entries:")
		 (org-agenda-view-columns-initially t)))
          (tags "project"
              ((org-agenda-overriding-header "Projects:")
               (org-tags-match-list-sublevels 'indented)))))
	))
