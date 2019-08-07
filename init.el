(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (manoj-dark)))
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 100)
 '(global-auto-revert-mode 1)
 '(global-display-line-numbers-mode t)
 '(grep-find-command
   (quote
    ("find . -name '*.java' -exec grep --color -nH --null -e  \\{\\} +" . 56)))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(magit-commit-arguments (quote ("--all")))
 '(package-selected-packages
   (quote
    (vterm js2-mode company-lsp lsp-ui lsp-mode diff-hl smartparens cider clojure-mode company beacon counsel-gtags flyspell-correct-ivy ivy-rich ivy-hydra smex flx counsel powerline org-bullets htmlize multiple-cursors which-key php-mode yaml-mode use-package typescript-mode tabbar-ruler popup neotree markdown-mode magit jtags highlight-parentheses feature-mode dired-sort-menu dired-sort dired+ csv-mode csharp-mode php-mode)))
 '(recentf-max-saved-items 30)
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
;; Language server:
;;   npm install -g typescript-language-server
;;   npm i -g typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :defer t)
;; Javascript mode
(use-package js2-mode
  :mode "\\.js\\'"
  :defer t
  :init
;;  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (setf js2-mode-indent-inhibit-undo t))
;; Markdown mode
(use-package markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t))
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
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))
;; Smartparens
(use-package smartparens
  :bind
  (("C-)" . sp-forward-slurp-sexp)
   ("C-(" . sp-forward-barf-sexp)
   ("C-{" . sp-backward-unwrap-sexp)
   ("C-}" . sp-unwrap-sexp)))
(show-paren-mode 1)
;; Clojure
(use-package clojure-mode
  :defer t
  :mode ("\\.cljs$" "\\.cljc$")
  :init
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode))
(use-package cider
  :defer t
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (setq cider-repl-display-help-banner nil))
;; Diff highlight
(use-package diff-hl
  :ensure t
  :init
  ;; On-the-fly diff updates
  (diff-hl-flydiff-mode)
  ;; Enable diff-hl globally
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
;; LSP mode
;; Install clojure-lsp from: https://github.com/snoe/clojure-lsp
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))
  :init
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-snippet nil)
  (setq flymake-start-on-flymake-mode nil)
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'js2-mode-hook #'lsp)
  (add-hook 'java-mode-hook #'lsp))
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-sideline-enable nil))
(use-package company-lsp
  :ensure t
  :commands company-lsp)
;; Vterm
(use-package vterm
  :ensure t
  :config
  ;; Turn off line numbers in vterm
  (add-hook 'vterm-mode-hook 'my/disable-line-numbers))
