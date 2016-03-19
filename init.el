(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(global-set-key (kbd "C-x g") 'magit-status)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq make-backup-files nil) ;; stop creating those backup~ files
;; tabbar
(require 'tabbar)
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
(require 'tabbar-ruler)
;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
;; Set initial window/frame size and position
(setq initial-frame-alist
	  `((top . 0)	
	    (left . 0)
	    (height . 36)
	    (width . 120)))
(setq default-frame-alist (copy-alist initial-frame-alist))
;; Parentheses highlight
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
(setq dired-listing-switches "-lXGha --group-directories-first")
;; YAML mode
 (require 'yaml-mode)
   (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; Omit mode
(require 'dired-x)
    (setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
;; Dired+
(setq diredp-hide-details-initially-flag nil)
(require 'dired+)
;; Typescript mode
(require 'typescript-mode)
;; Markdown mode
 (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

