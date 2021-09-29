;; Org mode
(defconst my-org-directory
  (if (string= (system-name) "WIN10R90H8MKJ")
      "//brutus/it/healthinformatics/BobN/org"
      "~/Dropbox/org"))
(defconst todo-org-file (concat my-org-directory "/todo.org"))
(defconst journal-org-file (concat my-org-directory "/journal.org.gpg"))
(defconst transfer-org-file "/media/sf_healthinformatics/BobN/org/transfer.org")
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-c!" 'org-time-stamp-inactive)

(setq org-agenda-start-day "-2d")
(setq org-agenda-span 7)
(setq org-agenda-start-on-weekday nil)

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
        ("x" "Transfer" entry (file+headline transfer-org-file "Tasks")
	       "* TODO %?\n  %i\n  %a\n%T")
	      ("j" "Journal" entry (file+olp+datetree journal-org-file)
	       "* %?\nEntered on %U\n  %i\n  %a"))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (ruby . t)
   (emacs-lisp . t)))
(setq org-agenda-include-diary t)
(setq org-confirm-babel-evaluate nil)
(cl-defun my/make-save-template (sbe what)
  `(lambda ()
     (interactive)
     (let (
         (oldp (point))
         (oldbuff (current-buffer)))
       (org-save-all-org-buffers)
       (find-file todo-org-file)
       (ignore-errors (org-sbe ,sbe))
       (unless (eq (current-buffer) oldbuff) (switch-to-buffer oldbuff))
       (goto-char oldp))
     (message (concat ,what " complete."))))

(global-set-key (kbd "C-c b") (my/make-save-template 'backup "Backup"))

(fset 'my-agenda
   (lambda (&optional arg) "Startup my custom agenda." (interactive "p") (kmacro-exec-ring-item (quote ("ap" 0 "%d")) arg)))
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
(setq org-agenda-include-diary nil)
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
