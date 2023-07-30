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

;;; Code:

(require 'elfeed)
(require 'org)

(defgroup elfeed-curate ()
  "Curate Elfeed content."
  :group 'comm)

;; Customizations

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

(defcustom elfeed-curate-org-content-title-function #'elfeed-curate-org-content-title--default
  "Function used to create the title content.
This will likely need to be changed for different export formats.
The default is for HTML output"
  :group 'elfeed-curate
  :type 'function)

(defcustom elfeed-curate-org-title "Content of Interest"
  "The TITLE part of the '<Date> <Title>' format.
See the `elfeed-curate-org-content-title--default` function."
  :group 'elfeed-curate
  :type 'string)

(defcustom elfeed-curate-org-html-options "#+OPTIONS: html-style:nil toc:nil num:nil f:nil html-postamble:nil html-preamble:nil"
  "Set html format options. Default is no styles, TOC, section numbering, footer."
  :group 'elfeed-curate
  :type 'string)

(defcustom elfeed-curate-export-dir "~/"
  "Export the org and exported (e.g. html) content to this directory."
  :group 'elfeed-curate
  :type 'directory)

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

(defcustom elfeed-curate-group-exclude-tag-list (list 'unread 'star elfeed-curate-annotation-tag)
  "List of tags to exclude from the group list.
These are typically non-subject categories."
  :group 'elfeed-curate
  :type '(repeat symbol))

;; Variables

(defvar elfeed-curate-exit-keys "C-c C-c"
  "Save the content from the recursive edit buffer to an entry annotation.")

(defvar elfeed-curate-abort-keys "C-c C-k"
  "Abort the recursive edit session without saving the annotation.")

(defvar elfeed-curate-org-file-name  "export.org"
  "Generated org file name.")

;; Code

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

(defun elfeed-curate-org-content-title--default (title)
  "Get the default TITLE content."
  (format "%s
#+BEGIN_EXPORT html
<h1>%s %s</h1>
#+END_EXPORT\n" elfeed-curate-org-html-options (elfeed-curate-current-date-string) title))

(defun elfeed-curate-concat-authors (entry)
  "Return a string of all authors concatenated for the given ENTRY."
  (let ((authors (elfeed-meta entry :authors)))
    (mapconcat
     (lambda (author) (plist-get author :name)) authors ", ")))

(defun elfeed-curate-get-entry-annotation (entry)
  "Get annotation from an ENTRY."
  (let ((annotation (elfeed-meta entry elfeed-curate-annotation-key)))
    (if annotation annotation "")))

(defun elfeed-curate-set-entry-annotation (entry annotation)
  "Set ANNOTATION on an ENTRY."
  (let* ((txt (if (= (length annotation) 0) nil annotation))
         (tag-func (if txt 'elfeed-tag 'elfeed-untag)))
    (elfeed-meta--put entry elfeed-curate-annotation-key txt)
    (funcall tag-func entry elfeed-curate-annotation-tag)
    (save-excursion
      (with-current-buffer (elfeed-search-buffer)
        (elfeed-search-update-entry entry)))))

(defun  elfeed-curate-add-org-entry (entry)
  "Add an elfeed ENTRY to the org buffer."
  (let* ((annotation (elfeed-curate-get-entry-annotation entry))
         (authors (elfeed-curate-concat-authors entry))
         (authors-str (if (= (length authors) 0) "" (concat " (" authors ")"))))
    (insert (format "- [[%s][%s]]%s\n"
                    (elfeed-entry-link entry)
                    (elfeed-entry-title entry)
                    authors-str))
    (when (> (length annotation) 0)
      (insert (format "%s\n" annotation)))))

(defun elfeed-curate-tag-to-group-name (tag)
  "Convert TAG to a human readable title string.
Split on '_' and capitalize each word. e.g. tag-name --> Tag Name"
  (capitalize (replace-regexp-in-string "_" " " (format "%s" tag))))

(defun elfeed-curate-add-org-group (title entries)
  "Add a group TITLE of elfeed ENTRIES to the org buffer."
  (insert (format "* %s\n" (elfeed-curate-tag-to-group-name title)))
  (dolist (entry entries)
    (elfeed-curate-add-org-entry entry)))

(defun elfeed-curate-group-org-entries (entries)
  "Create a plist of grouped ENTRIES."
  (let (groups)
    (dolist (entry entries)
      (let ((tags (elfeed-entry-tags entry)))
        (dolist (tag tags)
          (when (not (memq tag elfeed-curate-group-exclude-tag-list))
            (progn
              (when (not (plist-member groups tag))
                (setq groups (plist-put groups tag ())))
              (push entry (plist-get groups tag)))))))
    groups))

(defun elfeed-curate-elfeed-entry-count (groups)
  "Count total entries in all GROUPS."
  (let ((count 0))
    (dolist (key (elfeed-curate-plist-keys groups))
      (setq count (+ count (length (plist-get groups key)))))
    count))


(defun elfeed-curate-annotation-keymap ()
  "Create a keymap for the annotation buffer."
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd elfeed-curate-exit-keys) 'exit-recursive-edit)
    (define-key km (kbd elfeed-curate-abort-keys) 'abort-recursive-edit)
    km))

(defun elfeed-curate-edit-annotation (title default-string)
  "Edit annotation string for the group TITLE with the DEFAULT-STRING."
  (with-temp-buffer
    (org-mode)
    (when (fboundp 'org-superstar-mode) (org-superstar-mode))
    (when (fboundp 'org-modern-mode) (org-modern-mode))
    (when (fboundp 'prettify-symbols-mode) (prettify-symbols-mode))
    (setq buffer-read-only nil)
    (outline-show-all)
    (rename-buffer "CAPTURE-annotation.org" t)
    (insert default-string)
    (goto-char (point-min))
    (let ((title-str (propertize (concat "Annotate '" (elfeed-curate-truncate-string title elfeed-curate-title-length) "'")
                                 'face 'mode-line-buffer-id)))
      (setq header-line-format
          (list
           title-str
           " --> Save: '"
           '(:eval (propertize elfeed-curate-exit-keys 'face 'mode-line-emphasis))
           "', Abort: '"
           '(:eval (propertize elfeed-curate-abort-keys 'face 'mode-line-emphasis))
           "'")))
    (switch-to-buffer (current-buffer))
    (use-local-map (elfeed-curate-annotation-keymap))
    (recursive-edit)
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun elfeed-curate-edit-entry-annoation ()
  "Edit selected entry annotation."
  (interactive)
  (let* ((is-search (string-equal (buffer-name) (buffer-name (elfeed-search-buffer))))
         (entry (if is-search (elfeed-search-selected :single) elfeed-show-entry))
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
      (when elfeed-curate-org-content-title-function
        (insert (funcall elfeed-curate-org-content-title-function elfeed-curate-org-title)))
      (dolist (key (elfeed-curate-plist-keys groups))
        (elfeed-curate-add-org-group key (plist-get groups key)))
      (let ((out-file-name (elfeed-curate-export-file-name)))
        (delete-file out-file-name)
        (org-export-to-file elfeed-curate-org-export-backend out-file-name)
        (save-window-excursion
          (async-shell-command (concat "xdg-open " out-file-name)))
        (message "Exported %d Elfeed groups (%d total entries) to %s"
                 (/ (length groups) 2) (elfeed-curate-elfeed-entry-count groups) out-file-name)))))

(provide 'elfeed-curate)

;;; elfeed-curate.el ends here
