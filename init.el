(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(magit-commit-arguments (quote ("--all")))
 '(package-selected-packages
   (quote
    (htmlize multiple-cursors which-key php-mode yaml-mode use-package typescript-mode tabbar-ruler popup neotree markdown-mode magit jtags highlight-parentheses feature-mode dired-sort-menu dired-sort dired+ csv-mode csharp-mode php-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; use-package
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(use-package magit)
;; recent files
(setq linum-format "%d ")
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(global-set-key (kbd "C-x g") 'magit-status)
;; Neotree
(use-package neotree)
(global-set-key [f8] 'neotree-toggle)
(setq make-backup-files nil) ;; stop creating those backup~ files
;; tabbar
(use-package tabbar)
(tabbar-mode)
(setq tabbar-buffer-groups-function
       (lambda ()
         (list "All Buffers")))
(setq tabbar-buffer-list-function
       (lambda ()
         (remove-if
          (lambda(buffer)
            (find (aref (buffer-name buffer) 0) " *"))
          (buffer-list))))
;; tabbar-ruler
(setq tabbar-ruler-global-tabbar t)    ; get tabbar
;;(setq tabbar-ruler-global-ruler t)     ; get global ruler
;;(setq tabbar-ruler-popup-menu t)       ; get popup menu.
;;(setq tabbar-ruler-popup-toolbar t)    ; get popup toolbar
;;(setq tabbar-ruler-popup-scrollbar t)  ; show scroll-bar on mouse-move
(use-package tabbar-ruler)
;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
;; Set initial window/frame size and position
(setq initial-frame-alist
	  `((top . 40)	
	    (left . 40)
	    (height . 36)
	    (width . 120)))
(setq default-frame-alist (copy-alist initial-frame-alist))
;; Parentheses highlight
(use-package highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
;;(setq dired-listing-switches "--group-directories-first -lXGha")
(setq dired-listing-switches "-lXGha")
(use-package dired-sort)
(use-package dired-sort-menu)
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
(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; Omit mode
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;; Dired+
(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil))
;; Typescript mode
(use-package typescript-mode)
;; Markdown mode
(use-package markdown-mode)
 (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; Helm -- Seems like it gets in the way more than it helps. Disable for now.
;; (use-package helm)
;; (require 'helm-config)
;; (helm-mode 1)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; PHP mode
(use-package php-mode)
;; CSharp mode
(use-package csharp-mode)
 (setq auto-mode-alist
     (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;; toggle-truncate-lines key binding
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
;; package-list-packages key binding
(global-set-key (kbd "C-x p") 'package-list-packages)
;; Feature mode (Cucumber)
(use-package feature-mode)
;; jtags-mode
(use-package jtags)
;; CSV Mode
(use-package csv-mode)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
;; which-key
(use-package which-key)
(which-key-mode)
;; nXML mode customization
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
;; Org mode
(defconst org-mode-directory "~/Downloads/org-mode")
(defconst my-org-directory "~/org")
(defconst todo-org-file (concat my-org-directory "/todo.org"))
(defconst journal-org-file (concat my-org-directory "/journal.org"))
(if (file-directory-p org-mode-directory)
    (progn
      (setq load-path (cons (concat org-mode-directory "/lisp") load-path))
      (setq load-path (cons (concat org-mode-directory "/contrib/lisp") load-path))
      (require 'org-install))
    (require 'org))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file todo-org-file)))
(setq org-log-done t)
(setq org-agenda-files (list
			todo-org-file
			"~/Projects/emacs/shared.org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(p!)" "WAITING(w@/!)" "|" "DONE(d)")))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline todo-org-file "Tasks")
             "* TODO %?\n  %i\n  %a\n%T")
        ("j" "Journal" entry (file+datetree journal-org-file)
	 "* %?\nEntered on %U\n  %i\n  %a")))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (ruby . t)
   (emacs-lisp . t)))
(setq org-agenda-include-diary t)
(setq org-confirm-babel-evaluate nil)
(fset 'do-org-backup
      "\C-co\274\C-s: backup\C-n\C-n\C-a\C-c\C-c")
(define-key global-map "\C-cb" 'do-org-backup)
(use-package htmlize)
;; multiple-cursors
(use-package multiple-cursors
  :bind (("<f2>" . mc/mark-previous-like-this)
         ("S-<f2>" . mc/unmark-previous-like-this)
         ("<f3>" . mc/mark-next-like-this)
         ("S-<f3>" . mc/unmark-next-like-this)
         ("C-c <f2>" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("<ESC> <ESC>" . mc/keyboard-quit))
  :ensure t)
;; Display time
(setq display-time-default-load-average nil)
(display-time)
