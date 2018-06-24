;;; custom-example.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-example.el to custom.el and change the configurations,
;;;       then restart Emacs.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "user name")           ; User full name
;; (setq centaur-mail-address "user@email.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
;; (setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, emacs-china or tuna
;; (setq centaur-theme 'doom)                     ; Color theme: default, doom, dark, light or daylight
;; (setq centuar-company-enable-yas t)            ; Enable/disable yasnippet for company: t or nil
;; (setq centaur-emoji-enabled t)                 ; Enable/disable emoji: t or nil
;; (setq centaur-benchmark-enabled t)             ; Enable/disable initialization benchmark: t or nil

;; For Emacs devel
;; e.g. 24.5, 25.3 or 26.1 are releses, while 26.0.90 is a devel release.
;; (when (= emacs-minor-version 0)
;;   (setq package-user-dir (locate-user-emacs-file "elpa-devel"))
;;   (setq desktop-base-file-name ".emacs-devel.desktop")
;;   (setq desktop-base-lock-name ".emacs-devel.desktop.lock"))

;; You may add addtional configurations here
;; (custom-set-variables )
(setq make-backup-files nil) ;; stop creating those backup~ files
(require 'package)
;;(setq package-enable-at-startup nil)
;; (add-to-list 'package-archives
;;       '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; use-package
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; Disable line numbers
(defun my/disable-line-numbers (&optional dummy)
  (display-line-numbers-mode -1))
;; Load org-config
(load "~/.emacs.d/org-config.el")
;; Sunrise Commander
(add-to-list 'load-path "~/sunrise-commander")
(require 'sunrise-commander)
(require 'sunrise-x-buttons)
(require 'sunrise-x-modeline)
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
(setq sr-cursor-follows-mouse nil)
(define-key sr-mode-map [mouse-1]        nil)
(define-key sr-mode-map [mouse-movement] nil)
(global-set-key (kbd "C-x c") 'sunrise-cd)
;; toggle-truncate-lines key binding
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
;; package-list-packages key binding
(global-set-key (kbd "C-x p") 'package-list-packages)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;;; custom-example.el ends here
