;;; my-funcs.el -*- lexical-binding: t; -*-

(defun my/is-wsl ()
  (and (eq system-type 'gnu/linux)
       (getenv "WSLENV")))

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
