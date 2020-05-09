;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bob Nadler"
      user-mail-address "robert.nadler@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq display-time-default-load-average nil)
(display-time)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Treemacs
(global-set-key [f8] '+treemacs/toggle)
(after! treemacs
  (load! "./init-treemacs.el"))

(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x l") 'display-line-numbers-mode)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

;; Sunrise Commander
(after! sunrise
  (use-package! sunrise-buttons)
  (use-package! sunrise-modeline)
  (use-package! sunrise-popviewer)
  (setq sr-cursor-follows-mouse nil)
  ;;(define-key sr-mode-map [mouse-1]        nil)
  ;;(define-key sr-mode-map [mouse-movement] nil)
  )
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
(global-set-key (kbd "C-x c") 'sunrise-cd)

(setq make-backup-files nil) ;; stop creating those backup~ files

(setq-default dired-omit-files-p t) ; Buffer-local variable
(after! dired+
  (use-package! dired-x)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(setq diredp-hide-details-initially-flag nil)
(diredp-toggle-find-file-reuse-dir 1)

(defun my/disable-line-numbers (&optional _)
  (display-line-numbers-mode -1))

;; Set initial frame size and position
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
(setq frame-resize-pixelwise t)
(my/set-initial-frame)
(setq confirm-kill-emacs nil)
(grep-apply-setting 'grep-find-command '("find . -name '*.java' -exec grep -nH --null -e   \\{\\} +" . 48))
;; Org mode
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(after! org
  (load! "./org-config.el"))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
