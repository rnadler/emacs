(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (manoj-dark)))
 '(display-time-mode t)
 '(fill-column 100)
 '(global-auto-revert-mode 1)
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(magit-commit-arguments (quote ("--all")))
 '(package-selected-packages
   (quote
    (racket-mode sly company ivy-posframe beacon counsel-gtags flyspell-correct-ivy ivy-rich ivy-hydra smex flx counsel powerline-evil org-bullets htmlize multiple-cursors which-key php-mode yaml-mode use-package typescript-mode tabbar-ruler popup neotree markdown-mode magit jtags highlight-parentheses feature-mode dired-sort-menu dired-sort dired+ csv-mode csharp-mode php-mode)))
 '(recentf-max-saved-items 30)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(size-indication-mode 1)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 113 :width normal)))))
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
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
(use-package magit
   :defer t)
;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x l") 'display-line-numbers-mode)
;; Neotree
(use-package neotree
    :defer t)
(global-set-key [f8] 'neotree-toggle)
(add-hook 'neo-after-create-hook 'my/disable-line-numbers)
(setq make-backup-files nil) ;; stop creating those backup~ files
;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
;; Set initial frame size and position
(defun my/set-initial-frame ()
  (let* ((base-factor 0.70)
	(a-width (* (display-pixel-width) base-factor))
        (a-height (* (display-pixel-height) base-factor))
        (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
	(a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))
(setq frame-resize-pixelwise t)
(my/set-initial-frame)
;; Parentheses highlight
(use-package highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
;;(setq dired-listing-switches "--group-directories-first -lXGha")
(setq dired-listing-switches "-lXGha")
(use-package dired-sort
  :defer t)
(use-package dired-sort-menu
  :defer t)
;; Handle special switches for Remote ftp directories 
;;(add-hook
;; 'dired-before-readin-hook
;; (lambda ()
;;  (setq dired-actual-switches
;;	      (if (file-remote-p dired-directory)
;;			  "-lXGha"
;;			  "-lXGha --group-directories-first"))
;;  (message "default-directory=%s switches=%s" default-directory dired-actual-switches)
;;))
;; YAML mode
(use-package yaml-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; Omit mode
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;; Dired+ (requires manual installation of dired+-20170818.1411 into ./elpa)
(use-package dired+
   :defer 0.5
   :init
   (setq diredp-hide-details-initially-flag nil))
(diredp-toggle-find-file-reuse-dir 1)
;; Typescript mode
(use-package typescript-mode
  :defer t)
;; Markdown mode
(use-package markdown-mode
  :defer t)
(autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; PHP mode
(use-package php-mode
    :defer t)
;; CSharp mode
(use-package csharp-mode
  :defer t
  :mode  "\\.cs$")
;; toggle-truncate-lines key binding
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
;; package-list-packages key binding
(global-set-key (kbd "C-x p") 'package-list-packages)
;; Feature mode (Cucumber)
(use-package feature-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode)))
;; jtags-mode
(use-package jtags
    :defer t)
;; CSV Mode
(use-package csv-mode
  :defer t
  :mode  "\\.[Cc][Ss][Vv]\\'")
;; which-key
(use-package which-key
  :defer 0.1)
(which-key-mode)
;; nXML mode customization
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
;; Display time
(setq display-time-default-load-average nil)
(display-time)
;; multiple-cursors
(use-package multiple-cursors
  :defer t
  :bind (("<f2>" . mc/mark-previous-like-this)
         ("S-<f2>" . mc/unmark-previous-like-this)
         ("<f3>" . mc/mark-next-like-this)
         ("S-<f3>" . mc/unmark-next-like-this)
         ("C-c <f2>" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("<ESC> <ESC>" . mc/keyboard-quit)))
;; Powerline
(use-package powerline)
(powerline-default-theme)
;; Ivy
(load "~/.emacs.d/init-ivy.el")
;; Load org-config
(load "~/.emacs.d/org-config.el")
;; Sunrise Commander
(add-to-list 'load-path "~/.emacs.d/sunrise-commander")
(require 'sunrise-commander)
(require 'sunrise-x-buttons)
(require 'sunrise-x-modeline)
(require 'sunrise-x-popviewer)
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
(setq sr-cursor-follows-mouse nil)
(define-key sr-mode-map [mouse-1]        nil)
(define-key sr-mode-map [mouse-movement] nil)
(global-set-key (kbd "C-x c") 'sunrise-cd)
;; Menu/Tool/Scroll bars
(and (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and (bound-and-true-p horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
;; Never lose the cursor again
(when (display-graphic-p)
  (use-package beacon
    :diminish beacon-mode
    :hook (after-init . beacon-mode)))
;; Company mode
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
;; Sly
(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))
;; Racket mode
(use-package racket-mode
  :defer t
  :config
  (setq racket-program "/home/bobn/racket/bin/racket"))
