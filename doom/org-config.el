;; org-config.el -*- lexical-binding: t; -*-

(defconst todo-org-file (concat my/org-directory "/todo.org"))
(defconst journal-org-file (concat my/org-directory "/journal.org.gpg"))
(when (not (my/is-wsl))
    (defconst scripts-org-file (concat my/org-directory "/scripts.org"))
    (defconst meeting-org-file (concat my/org-directory "/meeting.org"))
    (defconst archive-org-file (concat my/org-directory "/archive.org"))
    (defconst ecobuilds-org-file (concat my/org-directory "/ecobuilds.org"))
)
(define-key global-map "\C-cl" 'org-store-link)
;;(define-key global-map "\C-c!" 'org-time-stamp-inactive)

(setq org-agenda-start-day "-2d")
(setq org-agenda-span 7)
(setq org-agenda-start-on-weekday nil)
(setq org-export-with-toc nil)

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

(cl-defun my/make/org-capture-template
   (shortcut heading &optional (no-todo nil) (description heading) (category heading) (scheduled t))
  "Quickly produce an org-capture-template."
  `(,shortcut ,description entry
	      (file+headline org-default-notes-file
			     ,(concat heading "\n#+CATEGORY: " category))
	      ,(concat "*" (unless no-todo " TODO") " %?\n"
			(when nil ;; this turned out to be a teribble idea.
			  ":PROPERTIES:\n:"
			  (if scheduled
			      "SCHEDULED: %^{Any time ≈ no time! Please schedule this task!}t"
			    "CREATED: %U")
			  "\n:END:") "\n\n ")
		:empty-lines 1 :time-prompt t))

;; https://github.com/sprig/org-capture-extension#set-up-handlers-in-emacs
(setq org-capture-templates
      (cons (my/make/org-capture-template "b" "Blog")
	    '(("t" "Todo" entry (file+headline todo-org-file "Inbox")
	       "* TODO %?\n  %i\n  %a\n%T")
        ;; javascript:location.href = 'org-protocol://capture?template=L&url=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title) + '&body='
        ("L" "Protocol Link" entry (file+headline todo-org-file "Inbox")
         "* [[%:link][%:description]] %i\n- %?\nCaptured: %U")
	      ("j" "Journal" entry (file+olp+datetree journal-org-file)
	       "* %?\nEntered on %U\n  %i\n  %a"))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (ruby . t)
   (emacs-lisp . t)))
(setq org-agenda-include-diary t)
(setq org-confirm-babel-evaluate nil)

(fset 'my-agenda
   (lambda (&optional arg) "Startup my custom agenda." (interactive "p") (kmacro-exec-ring-item (quote ("ap" 0 "%d")) arg)))
;; Custom agenda
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-compact-blocks t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-files (if (my/is-wsl)
                           (list todo-org-file "~/Projects/emacs/shared.org")
                         (list todo-org-file meeting-org-file ecobuilds-org-file)))
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-agenda-tags-todo-honor-ignore-options t)

(cl-defun my/gen-agenda-todo
   (tag heading &optional (sort '(alpha-up)))
  "Generate an agenda item"
  `(tags-todo ,tag
	      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
	       (org-agenda-overriding-header ,heading)
	       (org-agenda-view-columns-initially t)
	       (org-agenda-sorting-strategy ',sort))))

(setq my/task-groups `(("arch"      "ARCH tasks:")
                       ("orion"     "ORION tasks:")
                       ("air11"     "AIR11 tasks:")
                       ("ecobuilds" "ECO-BUILDS tasks:")
                       ("dev"       "DEV Unscheduled tasks:")))

(setq my/wsl-task-groups `(("blog"      "BLOG tasks:")
                           ("emacs"     "EMACS tasks:")
                           ("home"      "HOME tasks:")))

(defun my/task-groups-string (groups)
  (mapconcat (lambda (x) (car x)) groups "-"))

(defun my/todo-list (groups)
  (cl-loop for (tag heading) in groups
	collect (my/gen-agenda-todo tag heading)))

(defun my/p-agenda-projects (groups)
  (append '((agenda "" nil))
	  (my/todo-list groups)
	    `(,(my/gen-agenda-todo
                (format "-%s-TODO=\"DONE\"-TODO=\"CANCELLED\"" (my/task-groups-string groups))
                "Unscheduled TODO entries:" '(tag-up alpha-up)))))

(setq org-agenda-custom-commands
      `(("p" "Agenda and Projects" ,(my/p-agenda-projects
                                     (if (my/is-wsl)
                                         my/wsl-task-groups
                                       my/task-groups)))))

(setq org-ellipsis " ▼")
(add-hook 'org-mode-hook 'my/disable-line-numbers)
(add-hook 'org-mode-hook 'org-appear-mode)
(add-hook 'org-agenda-finalize-hook
          (lambda ()
            (remove-text-properties (point-min) (point-max) '(mouse-face t))
            (my/disable-line-numbers)))
;; Other stuff
(setq org-catch-invisible-edits 'show-and-error)
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
(add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("[ ]" . "☐") prettify-symbols-alist)
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
;; Copy org link to clipboard
(defun my/replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))
;; Based on https://emacs.stackexchange.com/a/60555/19347
(defun my/org-export-url ()
  "Extract URL from org-mode link (or current word) and add it to kill ring."
  (interactive)
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (url (org-element-property :raw-link link))
         (url (if (not url) (thing-at-point 'word 'no-properties) url)))
    (kill-new url)
    (message (concat "Copied: " (my/replace-in-string "%" "%%" url)))))
(global-set-key (kbd "C-x y") 'my/org-export-url)

;; Helper
(defun my/org-number-of-subentries (&optional pos match scope level)
  "Return number of subentries for entry at POS.
   MATCH and SCOPE are the same as for `org-map-entries', but
   SCOPE defaults to 'tree.
   By default, all subentries are counted; restrict with LEVEL."
  (interactive)
  (save-excursion
    (goto-char (or pos (point)))
    ;; If we are in the middle of an entry, use the current heading.
    (org-back-to-heading t)
    (let ((maxlevel (when (and level (org-current-level))
                      (+ level (org-current-level)))))
      (message "%s subentries"
               (1- (length
                    (delq nil
                          (org-map-entries
                           (lambda ()
                             ;; Return true, unless below maxlevel.
                             (or (not maxlevel)
                                 (<= (org-current-level) maxlevel)))
                           match (or scope 'tree)))))))))
