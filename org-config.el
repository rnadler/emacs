;; Org mode
(defconst org-mode-directory "~/Downloads/org-mode")
(defconst my-org-directory "~/org")
(defconst todo-org-file (concat my-org-directory "/todo.org"))
(defconst journal-org-file (concat my-org-directory "/journal.org"))
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
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
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
			"~/Projects/emacs/shared.org"))
(setq org-agenda-custom-commands
      '(("p" "Agenda and Projects"
         ((agenda "" nil)
          (tags "project"
              ((org-agenda-overriding-header "Projects")
               (org-tags-match-list-sublevels 'indented)))))
	))
