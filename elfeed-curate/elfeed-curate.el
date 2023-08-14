;;; elfeed-curate.el --- Elfeed entry curation  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Robert Nadler <robert.nadler@gmail.com>

;; Author: Robert Nadler <robert.nadler@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (elfeed "3.4.1"))
;; Keywords: news
;; URL: https://github.com/rnadler/elfeed-curate

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `elfeed-curate' is an add-on for `elfeed', an RSS reader for
;; Emacs. This package allows you to annotate and publish curated RSS
;; feed entries.
;;
;; See https://github.com/rnadler/elfeed-curate for usage details.

(require 'cl-lib)
(require 'elfeed)
(require 'org)

;;; Code:

(defgroup elfeed-curate ()
  "Curate Elfeed content."
  :group 'comm)

;;; Customizations:

(defcustom elfeed-curate-title-length 60
  "Maximum length of the entry title to show in the annotation edit buffer."
  :group 'elfeed-curate
  :type 'integer)

(defcustom elfeed-curate-annotation-key :my/annotation
  "Elfeed database meta data key to store annotations."
  :group 'elfeed-curate
  :type 'symbol)

(defcustom elfeed-curate-annotation-tag 'ann
  "Tag used to indicate that annotation has been added to an entry."
  :group 'elfeed-curate
  :type 'symbol)

(defcustom elfeed-curate-star-tag 'star
  "Tag used to indicate that annotation has been `starred`."
  :group 'elfeed-curate
  :type 'symbol)

(defcustom elfeed-curate-no-group-tag 'no_category
  "Tag used to indicate that an entry has no group tag.
The entry will be added to this group in the export.
Set to nil to not display these entries."
  :group 'elfeed-curate
  :type 'symbol)

(defcustom elfeed-curate-org-content-header-function #'elfeed-curate-org-content-header--default
  "Function used to create the header (options and title) content.
The default is for HTML output."
  :group 'elfeed-curate
  :type 'function)

(defcustom elfeed-curate-org-title "Content of Interest"
  "The TITLE part of the 'DATE TITLE' format.
See the `elfeed-curate-org-content-header--default` function."
  :group 'elfeed-curate
  :type 'string)

(defcustom elfeed-curate-org-options "html-style:nil toc:nil num:nil f:nil html-postamble:nil html-preamble:nil"
  "Set format options. Default is for an HTML export: no styles, TOC, section numbering, footer."
  :group 'elfeed-curate
  :type 'string)

(defcustom elfeed-curate-export-dir "~/"
  "Export the org and exported (e.g. html) content to this directory."
  :group 'elfeed-curate
  :type 'directory)

(defcustom elfeed-curate-show-group-count t
  "Flag to enable showing the count of each group in the exported output."
  :group 'elfeed-curate
  :type 'boolean)

(defcustom elfeed-curate-org-export-backend 'html
  "Select export format. Can be one of:
ascii - Export to plain ASCII text.
html - Export to HTML.
latex - Export to LaTeX.
md - Export to Markdown.
odt - Export to OpenDocument Text.
pdf - Export to PDF (requires additional setup)."
  :group 'elfeed-curate
  :type '(choice (const ascii) (const html) (const latex) (const md) (const odt) (const pdf)))

(defcustom elfeed-curate-group-exclude-tag-list (list 'unread elfeed-curate-star-tag elfeed-curate-annotation-tag)
  "List of tags to exclude from the group list.
These are typically non-subject categories."
  :group 'elfeed-curate
  :type '(repeat symbol))

;;; Variables:

(defvar elfeed-curate-exit-keys "C-c C-c"
  "Save the content from the recursive edit buffer to an entry annotation.")

(defvar elfeed-curate-delete-keys "C-c C-d"
  "Delete the content from the recursive edit buffer and abort the edit session.")

(defvar elfeed-curate-abort-keys "C-c C-k"
  "Abort the recursive edit session without saving the annotation.")

(defvar elfeed-curate-org-file-name  "export.org"
  "Generated org file name.")

(defvar elfeed-curate-capture-buffer-name "CAPTURE-annotation.org"
  "Annotation capture buffer name.")

;;; Functions:

(defun elfeed-curate-plist-keys (plist)
  "Return a list of keys from the given property list PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun elfeed-curate-truncate-string (string limit)
  "Truncate a STRING to a given LIMIT."
  (if (< (length string) limit)
      string
    (substring string 0 limit)))

(defun elfeed-curate-export-file-extension ()
  "Extension of the exported file."
  (format "%s" elfeed-curate-org-export-backend))

(defun elfeed-curate--org-file-path ()
  "File path for the generated org file."
  (concat elfeed-curate-export-dir elfeed-curate-org-file-name))

(defun elfeed-curate-current-date-string ()
  "The current date string."
  (format-time-string "%d-%B-%Y" (current-time)))

(defun elfeed-curate-export-file-name ()
  "Exported file name."
  (format "%s%s-export.%s" elfeed-curate-export-dir (elfeed-curate-current-date-string) (elfeed-curate-export-file-extension)))

(defun elfeed-curate-org-content-header--default (title)
  "Get the default header (options and TITLE) content."
  (format "#+OPTIONS: %s
#+TITLE: %s %s\n"
          elfeed-curate-org-options
          (elfeed-curate-current-date-string) title))

(defun elfeed-curate-concat-authors (entry)
  "Return a string of all authors concatenated for the given ENTRY."
  (let ((authors (elfeed-meta entry :authors)))
    (mapconcat
     (lambda (author) (plist-get author :name)) authors ", ")))

(defun elfeed-curate-concat-other-groups (entry group)
  "Return a string of all other groups (not GROUP) concatenated for the given ENTRY."
  (let* ((tags (elfeed-entry-tags entry))
         (tags (delq group tags))
         (tags (cl-remove-if (lambda (tag) (memq tag elfeed-curate-group-exclude-tag-list)) tags)))
    (mapconcat
     (lambda (tag) (elfeed-curate-tag-to-group-name tag)) tags ", ")))

(defun elfeed-curate-get-entry-annotation (entry)
  "Get annotation from an ENTRY."
  (let ((annotation (elfeed-meta entry elfeed-curate-annotation-key)))
    (if annotation annotation "")))

(defun elfeed-curate--show-entry (msg entry tag)
  "DEBUG: Show an ENTRY with MSG."
  (message (format "%s [%s]: %s tags: %s" msg tag
                   (if entry (elfeed-entry-title entry) "?")
                   (if entry (elfeed-entry-tags entry) "?"))))

(defun elfeed-curate--update-tag (entry tag add-tag)
  "Update the TAG on an ENTRY. ADD-TAG determine whether to tag or untag."
  (let ((tag-func (if add-tag 'elfeed-tag 'elfeed-untag)))
    (funcall tag-func entry tag)
    (save-excursion
      (with-current-buffer (elfeed-search-buffer)
        (elfeed-search-update-entry entry)))))

(defun elfeed-curate-set-entry-annotation (entry annotation)
  "Set ANNOTATION on an ENTRY."
  (let ((txt (if (= (length annotation) 0) nil annotation)))
    ;;(elfeed-meta--put entry elfeed-curate-annotation-key txt)
    (setf (elfeed-entry-meta entry)
          (plist-put (elfeed-entry-meta entry) elfeed-curate-annotation-key txt))
    (elfeed-curate--update-tag entry elfeed-curate-annotation-tag txt)))

(defun  elfeed-curate-add-org-entry (entry group)
  "Add an elfeed ENTRY in GROUP to the org buffer."
  (let* ((annotation (elfeed-curate-get-entry-annotation entry))
         (authors (elfeed-curate-concat-authors entry))
         (authors-str (if (= (length authors) 0) "" (concat " (" authors ")")))
         (other-groups (elfeed-curate-concat-other-groups entry group))
         (groups-str (if (= (length other-groups) 0) "" (concat " **[" other-groups "]**"))))
    (insert (format "- [[%s][%s]]%s%s\n"
                    (elfeed-entry-link entry)
                    (elfeed-entry-title entry)
                    authors-str groups-str))
    (when (> (length annotation) 0)
      ; Try to keep annotation content under the entry link.
      (insert (format "  %s\n"
                      (replace-regexp-in-string "\n" "\n  " annotation))))))

(defun elfeed-curate-tag-to-group-name (tag)
  "Convert TAG to a human readable title string.
Split on '_' and capitalize each word. e.g. tag-name --> Tag Name"
  (capitalize (replace-regexp-in-string "_" " " (format "%s" tag))))

(defun elfeed-curate-add-org-group (group entries)
  "Add a GROUP of elfeed ENTRIES to the org buffer."
  (let ((count-str (if elfeed-curate-show-group-count
                       (format " (%d)" (length entries)) "")))
    (insert (format "* %s%s\n" (elfeed-curate-tag-to-group-name group) count-str)))
  (dolist (entry entries)
    (elfeed-curate-add-org-entry entry group)))

(defmacro elfeed-curate--add-entry-to-group (groups entry tag)
  "Add an ENTRY to the GROUPS plist with the group TAG."
  `(progn
     (when (not (plist-member ,groups ,tag))
       (setq ,groups (plist-put ,groups ,tag ())))
     (push ,entry (plist-get ,groups ,tag))))

(defun elfeed-curate-group-org-entries (entries)
  "Create a plist of grouped ENTRIES."
  (let (groups)
    (dolist (entry entries)
      (let ((tags (elfeed-entry-tags entry))
            (pushed))
        (cl-dolist (tag tags)
          (when (not (memq tag elfeed-curate-group-exclude-tag-list))
            (progn
              (elfeed-curate--add-entry-to-group groups entry tag)
              (setq pushed t)
              (cl-return)))) ; An entry is only added to one group
        (when (and (not pushed) elfeed-curate-no-group-tag)
          (elfeed-curate--add-entry-to-group groups entry elfeed-curate-no-group-tag))))
      groups))

(defun elfeed-curate-elfeed-entry-count (groups)
  "Count total entries in all GROUPS."
  (let ((count 0))
    (dolist (key (elfeed-curate-plist-keys groups))
      (cl-incf count (length (plist-get groups key))))
    count))

(defun elfeed-curate--annotation-keymap ()
  "Create a keymap for the annotation buffer."
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd elfeed-curate-exit-keys) 'exit-recursive-edit)
    (define-key km (kbd elfeed-curate-abort-keys) 'abort-recursive-edit)
    (define-key km (kbd elfeed-curate-delete-keys)
                (lambda ()
                  (interactive)
                  (erase-buffer)
                  (exit-recursive-edit)))

    km))

(defmacro elfeed-curate--key-emphasis (keys)
  "Propertize the given KEYS with emphasis."
  `'(:eval (propertize ,keys 'face 'mode-line-emphasis)))

(defun elfeed-curate-edit-annotation (title default-string)
  "Edit annotation string for the group TITLE with the DEFAULT-STRING.
Returns the annotation buffer content."
  (with-temp-buffer
    (org-mode)
    (setq buffer-read-only nil)
    (outline-show-all)
    (rename-buffer elfeed-curate-capture-buffer-name t)
    (insert default-string)
    (goto-char (point-max))
    (let ((title-str (propertize (concat "Annotate '" (elfeed-curate-truncate-string title elfeed-curate-title-length) "'")
                                 'face 'mode-line-buffer-id)))
      (setq header-line-format
          (list
           title-str
           " --> Save: '" (elfeed-curate--key-emphasis elfeed-curate-exit-keys)
           "', Delete: '" (elfeed-curate--key-emphasis elfeed-curate-delete-keys)
           "', Abort: '"  (elfeed-curate--key-emphasis elfeed-curate-abort-keys)
           "'")))
    (switch-to-buffer (current-buffer))
    (use-local-map (elfeed-curate--annotation-keymap))
    (recursive-edit)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun elfeed-curate--get-entry ()
  "Gets the current entry from either the search or show buffer."
  (let ((is-search (string-equal (buffer-name) (buffer-name (elfeed-search-buffer)))))
    (if is-search (elfeed-search-selected :single) elfeed-show-entry)))

(defun elfeed-curate--open-in-external-app (fname)
  "Open FNAME in external app.
Simplified version of: `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html`"
  (interactive)
  (let ((xfileList (list (expand-file-name fname))))
    (cond
     ((string-equal system-type "windows-nt")
      (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
            (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
        (mapc
         (lambda (x)
           (message "%s" x)
           (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (if (string-match "'" x) (replace-match "`'" t t x) x))) nil)))
         xfileList)))
     ((string-equal system-type "darwin")
      (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
     ((string-equal system-type "gnu/linux")
      (mapc (lambda (xfpath)
              (call-process shell-file-name nil 0 nil
                            shell-command-switch
                            (format "%s %s" "xdg-open" (shell-quote-argument xfpath)))) xfileList))
     ((string-equal system-type "berkeley-unix")
      (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList)))))

;;;###autoload
(defun elfeed-curate-reconcile-annotations ()
  "Ensure all database entries have the correct annotation tags.
1. Add the specified annotation tag if annotation exists.
2. Remove annotation tag if annotation does not exist."
  (interactive)
  (let ((add-count 0)
        (remove-count 0)
        (total-count 0)
        (ann-count 0))
    (with-elfeed-db-visit (entry _)
      (let ((has-ann (/= (length (elfeed-curate-get-entry-annotation entry)) 0))
            (has-tag (elfeed-tagged-p elfeed-curate-annotation-tag entry)))
        (cl-incf total-count)
        (cond
         ((and has-ann has-tag)
          (cl-incf ann-count))
         ((and has-ann (not has-tag))
          (cl-incf add-count)
          (cl-incf ann-count)
          (elfeed-curate--update-tag entry elfeed-curate-annotation-tag t))
         ((and has-tag (not has-ann))
          (cl-incf remove-count)
          (elfeed-curate--update-tag entry elfeed-curate-annotation-tag nil)))))
    (message (format "Annotation tags reconciled for %d entries: %d added, %d removed, %d total annoations"
                     total-count add-count remove-count ann-count))))

;;;###autoload
(defun elfeed-curate-toggle-star ()
  "Toggle `elfeed-curate-star-tag' on the current entry in either the search or show buffer."
  (interactive)
  (let* ((entry (elfeed-curate--get-entry))
         (add-tag (not (memq elfeed-curate-star-tag (elfeed-entry-tags entry)))))
    (elfeed-curate--update-tag entry elfeed-curate-star-tag add-tag)))

;;;###autoload
(defun elfeed-curate-edit-entry-annoation ()
  "Edit selected entry annotation."
  (interactive)
  (let* ((entry (elfeed-curate--get-entry))
         (current-annotation (elfeed-curate-get-entry-annotation entry))
         (new-annotation (elfeed-curate-edit-annotation (elfeed-entry-title entry) current-annotation)))
    (when (not (string-equal new-annotation current-annotation))
      (elfeed-curate-set-entry-annotation entry new-annotation))))

;;;###autoload
(defun elfeed-curate-export-entries ()
  "Write all displayed Elfeed entries to an export file."
  (interactive)
  (let* ((groups (elfeed-curate-group-org-entries elfeed-search-entries))
         (org-file (expand-file-name (elfeed-curate--org-file-path))))
    (with-temp-file org-file
      (when (functionp elfeed-curate-org-content-header-function)
        (insert (funcall elfeed-curate-org-content-header-function elfeed-curate-org-title)))
      (dolist (group (elfeed-curate-plist-keys groups))
        (elfeed-curate-add-org-group group (plist-get groups group)))
      (let ((out-file-name (elfeed-curate-export-file-name)))
        (delete-file out-file-name)
        (org-export-to-file elfeed-curate-org-export-backend out-file-name)
        (elfeed-curate--open-in-external-app out-file-name)
        (message "Exported %d Elfeed groups (%d total entries) to %s"
                 (length (elfeed-curate-plist-keys groups)) (elfeed-curate-elfeed-entry-count groups) out-file-name)))))

(provide 'elfeed-curate)

;;; elfeed-curate.el ends here