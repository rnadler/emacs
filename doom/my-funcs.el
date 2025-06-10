;;; my-funcs.el -*- lexical-binding: t; -*-

(defun my/is-wsl ()
  (string= (system-name) "ubuntu-bobn"))

(defun my/is-k8s-machine ()
  (or
   (string= (system-name) "bobn-ubuntu-2404")
   (my/is-wsl)))

(defun my/diff-config
    (file)
  (ediff (concat "~/Projects/emacs/doom/" file) (concat "~/.doom.d/" file)))

;; https://www.emacs.dyerdwelling.family/emacs/20230503211610-emacs--isearch-occur-advice-window-focus/
;; The advice-add method doesn't seem to work!
;; The hook-based approach (in comment) works!
(defun my/rename-and-select-occur-buffer ()
  "Renames the current buffer to *Occur: [term] [buffer]*.
   Meant to be added to `occur-hook'."
  (cl-destructuring-bind (search-term _ (buffer-name &rest _)) occur-revert-arguments
    (pop-to-buffer
     (rename-buffer (format "*Occur: %s %s*" search-term buffer-name) t))))
(add-hook 'occur-hook #'my/rename-and-select-occur-buffer)

(defun my/disable-line-numbers (&optional _)
  (display-line-numbers-mode -1))

;; Set initial frame size and position

(defun my/frame-recenter (&optional frame)
"Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

(defun my/set-initial-frame ()
  (let* ((base-factor 0.70)
         (geometry (assq 'geometry (car (last (display-monitor-attributes-list)))))
         (fx (nth 1 geometry))
         (fy (nth 2 geometry))
	       (fheight (nth 4 geometry))
	       (fwidth (nth 3 geometry))
	       (a-width (* fwidth base-factor))
	       (a-height (* fheight base-factor))
	       (a-left (truncate (+ fx (/ (- fwidth a-width) 2))))
	       (a-top (truncate (+ fy (/ (- fheight a-height) 2)))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))

;; https://www.emacs.dyerdwelling.family/emacs/20240305160708-emacs--unified-interface-for-switching-contexts/
;; 05-Mar-2024
(defun my/switch-to-thing ()
  "Switch to a buffer, open a recent file, jump to a bookmark, or change the theme from a unified interface."
  (interactive)
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (recent-files recentf-list)
         (bookmarks (bookmark-all-names))
         (themes (custom-available-themes))
         (all-options (append buffers recent-files bookmarks
                              (mapcar (lambda (theme) (concat "Theme: " (symbol-name theme))) themes)))
         (selection (completing-read "Switch to: "
                                     (lambda (str pred action)
                                       (if (eq action 'metadata)
                                           '(metadata . ((category . file)))
                                         (complete-with-action action all-options str pred)))
                                     nil t nil 'file-name-history)))
    (pcase selection
      ((pred (lambda (sel) (member sel buffers))) (switch-to-buffer selection))
      ((pred (lambda (sel) (member sel bookmarks))) (bookmark-jump selection))
      ((pred (lambda (sel) (string-prefix-p "Theme: " sel)))
       (load-theme (intern (substring selection (length "Theme: "))) t))
      (_ (find-file selection)))))
(global-set-key (kbd "C-x t") 'my/switch-to-thing)

;; Rename buffer file
(defun my/rename-current-file ()
  "Rename the current visiting file and switch buffer focus to it."
  (interactive)

  (if (null (buffer-file-name))
      (user-error "Buffer does not have a filename: %s" (current-buffer)))
  (let ((new-filename (my/expand-filename-prompt
    		       (format "Rename %s to: " (file-name-nondirectory (buffer-file-name))))))
    (if (null (file-writable-p new-filename))
    	(user-error "New file not writable: %s" new-filename))

    (rename-file (buffer-file-name) new-filename 1)
    (find-alternate-file new-filename)
    (message "Renamed to and now visiting: %s" (abbreviate-file-name new-filename))))

;; https://www.emacs.dyerdwelling.family/emacs/20231013153639-emacs--more-flexible-duplicate-thing-function/
(defun my/dired-duplicate-file (arg)
  "Duplicate a file from dired with an incremented number.
If ARG is provided, it sets the counter."
  (interactive "p")
  (let* ((file (dired-get-file-for-visit))
          (dir (file-name-directory file))
          (name (file-name-nondirectory file))
          (base-name (file-name-sans-extension name))
          (extension (file-name-extension name t))
          (counter (if arg (prefix-numeric-value arg) 1))
          (new-file))
    (while (and (setq new-file
                  (format "%s%s_%03d%s" dir base-name counter extension))
             (file-exists-p new-file))
      (setq counter (1+ counter)))
    (if (file-directory-p file)
      (copy-directory file new-file)
      (copy-file file new-file))
    (dired-revert)))

;; https://www.emacs.dyerdwelling.family/emacs/20240728141344-emacs--sending-dired-directories-to-meld/
(require 'cl-lib)
(defun my/dired-meld-diff-all-dwim ()
"Compare all marked directories in all visible Dired buffers using Meld.
   The order of directories respects the order suggested by `dired-dwim-target`."
(interactive)
(let ((files ()))
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'dired-mode)
              (dired-get-marked-files))
        (setq files (append files (dired-get-marked-files))))))
  (if (or (<= (length files) 1)
        (not (cl-every 'file-directory-p files)))
    (message "Please mark at least two directories.")
    (apply 'start-process "meld" nil "meld" files))))

;; The key binding doesn't seem to work...
;;(define-key dired-mode-map (kbd "C-c d") 'my/dired-duplicate-file)

(defun my/expand-filename-prompt (prompt)
  "Return expanded filename prompt."
  (expand-file-name (read-file-name prompt)))

;; Org link to dired

(defun my/org-open-file-with-dired (path)
  "Open in dired."
  (let ((d (file-name-directory path))
        (f (file-name-nondirectory path)))
    (dired d)
    (goto-char (point-min))
    (search-forward f nil t)))

(defalias '/rename 'my/rename-current-file)

(defun my/make-latest-version-of-GT ()
  "Make latest version of GT."
  (interactive)
  (if (file-exists-p "/tmp/myGT")
      (async-shell-command "cd /tmp/myGT; cd */.; ./glamoroustoolkit")
      (async-shell-command "mkdir /tmp/myGT; cd /tmp/myGT; wget https://dl.feenk.com/gt/GlamorousToolkitLinux64-release.zip; unzip GlamorousToolkitLinux64-release.zip; cd */.; ./glamoroustoolkit")))

;; Backups
(cl-defun my/make-save-template (sbe what)
  `(lambda ()
     (interactive)
     (let (
         (oldp (point))
         (oldbuff (current-buffer))
         (result ""))
       (org-save-all-org-buffers)
       (find-file (if (my/is-wsl) todo-org-file scripts-org-file))
       (setq result (ignore-errors (org-sbe ,sbe)))
       (unless (eq (current-buffer) oldbuff) (switch-to-buffer oldbuff))
       (goto-char oldp)
       (when (string= (buffer-name) "*Org Agenda*") (org-agenda-redo-all))
       (message (concat ,what " complete: " result)))))

(global-set-key (kbd "C-c b") (my/make-save-template 'backup "Backup"))
(if (not (my/is-wsl))
    (global-set-key (kbd "C-c d") (my/make-save-template 'save_git_diff "Save git diff")))

(defconst my/org-directory "~/org")

(defconst my/howm-task-file
  (if (my/is-wsl)
      "2024-12-23-091518.org"
    "2024-12-13-135935.org"))

(defun my/kill-all-dired-buffers ()
  "Kill all Dired buffers"
  (interactive)
  (let ((kill-count 0))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (eq (buffer-local-value 'major-mode buffer) 'dired-mode))
        (kill-buffer buffer)
        (setq kill-count (1+ kill-count))))
    (message "Killed %d Dired buffer(s)" kill-count)))

(global-set-key (kbd "C-c x") 'my/kill-all-dired-buffers)

(defun my/toggle-elfeed-curate-export-backend ()
  "Toggle the elfeed-curate-org-export-backend between 'html and 'md"
  (interactive)
  (if (eq elfeed-curate-org-export-backend 'html)
      (setq elfeed-curate-org-export-backend 'md)
    (setq elfeed-curate-org-export-backend 'html))
  (message "elfeed-curate-org-export-backend is now '%s."
           (symbol-name elfeed-curate-org-export-backend)))

(defun my/open-projects-in-vterm ()
  (interactive)
  (let ((dirs '("~"
                "~/Projects/dhp-migration"
                "~/Projects/avw-patient-migration-worker"
                "~/Projects/airview-migration-service")))
    (dolist (dir dirs)
      (let* ((expanded-dir (file-name-as-directory (expand-file-name dir)))
             (dir-name (file-name-nondirectory (directory-file-name expanded-dir)))
             (buffer-exists
              (seq-some (lambda (buf)
                          (string-match-p dir (buffer-name buf))) (buffer-list))))
        (unless buffer-exists
          (let ((default-directory expanded-dir)
                (buffer-name (format "vterm: %s" dir-name)))
            (vterm (generate-new-buffer-name buffer-name))))))))

;; https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/
(defun my/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))
(global-set-key [remap keyboard-quit] #'my/keyboard-quit-dwim)

(defun my/decode-base64-region (start end)
  "Decode base64 of a selected region"
  (interactive "r")
  (let* ((raw (buffer-substring-no-properties start end))
         (clean (replace-regexp-in-string "^['\"]\\|['\"]$" "" raw))
         (decoded (base64-decode-string clean)))
    (message decoded)))
