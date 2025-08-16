(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(column-number-mode t)
 '(custom-enabled-themes '(wheatgrass))
 '(display-time-mode t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fill-column 100)
 '(global-auto-revert-mode 1)
 '(global-display-line-numbers-mode t)
 '(grep-find-command
   '("find . -name '*.java' -exec grep --color -nH --null -e  \\{\\} +" . 56))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(package-selected-packages nil)
 '(server-auth-key
   "N#'=2;T_VbOS#<,u~$bue@j1_C=n{/x'#'^vW532`5'OIYkRWGIUxWD.#]g$CC<U")
 '(server-host "0.0.0.0")
 '(server-port "8081")
 '(server-use-tcp t)
 '(size-indication-mode 1)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 113 :width normal))))
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c")))))

(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; Bootstrap `use-package'
(setq-default use-package-always-ensure t ; Auto-download package if not exists
              use-package-always-defer t ; Always defer load package to speed up startup
              use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
;; Disable line numbers
(defun my/disable-line-numbers (&optional dummy)
  (display-line-numbers-mode -1))
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; use-package
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq org-hide-emphasis-markers t)
(setq confirm-kill-processes nil)

(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x l") 'display-line-numbers-mode)
(setq make-backup-files nil) ;; stop creating those backup~ files
;; Display time
(setq display-line-numbers-type t)
(setq display-time-default-load-average nil)
(display-time)
(setq pixel-scroll-precision-mode t)
;; Menu/Tool/Scroll bars
(and (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (bound-and-true-p horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
;; Dired
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-movement-style 'cycle)  ;; `cycle' or `bounded'
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-align-to-header t)
(add-hook 'dired-mode-hook 'auto-revert-mode)
(add-hook 'dired-mode-hook 'my/disable-line-numbers)
(setq dired-listing-switches "-alh --group-directories-first")
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-use-localized-time-format t)
(set-frame-font "Consolas-11" t t)

;; Recent files

(use-package recentf
  :init
  (setq recentf-max-saved-items 100
	recentf-max-menu-items 50))
(require 'recentf)
(recentf-mode 1)
(defun my/recentf-track-opened-dirs ()
  "Add Dired directories to `recentf-list'."
  (when (and (eq major-mode 'dired-mode)
             buffer-file-name)
    (recentf-add-file buffer-file-name)))
(defun my/recentf-track-dired-dir ()
  "Add current Dired directory to `recentf-list'."
  (when (eq major-mode 'dired-mode)
    (recentf-add-file (expand-file-name default-directory))))
(add-hook 'dired-mode-hook 'my/recentf-track-dired-dir)

(setq recentf-exclude '("\\.gz\\'" "\\.xz\\'" "\\.zip\\'")) ;; don’t exclude dirs
(defun my/recentf-vertico ()
  "Find a recent file using Vertico + completing-read."
  (interactive)
  (when (require 'recentf nil t)
    (unless recentf-mode (recentf-mode 1))
    (let ((file (completing-read "Recent file: " recentf-list nil t)))
      (when file
        (find-file file)))))
(global-set-key (kbd "C-x C-r") 'my/recentf-vertico)

;; Vertico
(use-package vertico
    :init
    (vertico-mode))


;; Corfu
(use-package corfu
  :custom
  (corfu-scroll-margin 10)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))
;; cape
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(which-key-mode)
