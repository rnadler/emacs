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
;;
;;(setq doom-font (font-spec :family "monospace" :size 14))
;;
(setq doom-font (font-spec :family "Ubuntu Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 16)
      doom-big-font (font-spec :family "Ubuntu Mono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))


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
(setq confirm-kill-processes nil)

;; Treemacs
;;(global-set-key [f8] 'treemacs)
;;(after! treemacs
;;  (load! "./init-treemacs.el"))

(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x l") 'display-line-numbers-mode)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

;; Elfeed
(global-set-key (kbd "C-x w") 'elfeed)
(after! elfeed
  (setq-default elfeed-search-filter "@4-days-old +unread ")
  (setq elfeed-sort-order 'ascending))

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

(add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))

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
  (load! "./org-config.el")
  (setq org-execute-file-search-functions nil))

;; Beacon
(beacon-mode 1)

;; Org-roam
;; https://org-roam.github.io/org-roam/manual/Installation-_00281_0029.html#Installation-_00281_002
;; WSL chrome startup
(defconst chrome-exe "/mnt/c/Program Files (x86)/Google/Chrome/Application/chrome.exe")
(defconst roam-host "http://localhost:8080")

(defun my/is-wsl ()
  (string= (system-name) "WIN10R90H8MKJ"))

(when (my/is-wsl)
  (setq browse-url-generic-program chrome-exe))

(defun my/open-org-roam-server (_)
  (interactive)
  (when (not (get-buffer "*httpd*"))
    (org-roam-server-mode))
  (if (my/is-wsl)
      (browse-url-generic roam-host)
    (browse-url roam-host)))

(after! org-roam
  (setq org-roam-link-title-format "§%s")
  (when (string= (system-name) "bob-x1-carbon")
    (setq org-roam-directory "~/Dropbox/org/roam"))
  (setq org-roam-server-host "0.0.0.0")
  (setq org-roam-server-port 8080)
  (setq org-roam-graph-viewer 'my/open-org-roam-server)
  (setq org-roam-graph-extra-config '(("overlap" . "false")))
  ;; javascript:location.href = 'org-protocol://roam-ref?template=r&ref=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title)
  (setq org-roam-capture-ref-templates '(("r" "ref" plain #'org-roam-capture--get-point
                                          "#+roam_tags: %?\n* [[${ref}][${title}]]\n- "
                                          :file-name "${slug}"
                                          :head "#+title: ${title}\n#+DATE: %<%Y-%m-%d %H:%M:%S>\n#+roam_key: ${ref}\n#+category: webref\n"
                                          :unnarrowed t)))
  )
;; Clojure
;; https://github.com/ericdallo/dotfiles/blob/master/.doom.d/config.el#L99-L134
(use-package! cider
  :after clojure-mode
  :config
  (setq cider-ns-refresh-show-log-buffer t
        cider-show-error-buffer t;'only-in-repl
        cider-font-lock-dynamically '(macro core function var deprecated)
        cider-prompt-for-symbol nil)
  (set-lookup-handlers! 'cider-mode nil))

(use-package! clj-refactor
  :after clojure-mode
  :config
  (set-lookup-handlers! 'clj-refactor-mode nil)
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-add-ns-to-blank-clj-files nil
        ;; cljr-magic-require-namespaces
        ;; '(("s"   . "schema.core")
        ;;   ("th"  . "common-core.test-helpers")
        ;;   ("gen" . "common-test.generators")
        ;;   ("d-pro" . "common-datomic.protocols.datomic")
        ;;   ("ex" . "common-core.exceptions")
        ;;   ("dth" . "common-datomic.test-helpers")
        ;;   ("t-money" . "common-core.types.money")
        ;;   ("t-time" . "common-core.types.time")
        ;;   ("d" . "datomic.api")
        ;;   ("m" . "matcher-combinators.matchers")
        ;;   ("pp" . "clojure.pprint"))
  ))

(use-package! clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-thread-all-but-last t
        clojure-align-forms-automatically t
        yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-c"))

;; LSP
(use-package! lsp-mode
  :commands lsp
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         ;;(java-mode . lsp)
         )
  :config
  (setq ;; clojure-lsp is on PATH
        ;; lsp-clojure-custom-server-command '("bash" "-c" "/usr/bin/clojure-lsp")
        lsp-headerline-breadcrumb-enable t
        ;; lsp-lens-enable t
        ;; lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        lsp-signature-auto-activate nil
        lsp-completion-use-last-result nil))

(use-package! lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-list-width 60
        ;; lsp-ui-doc-enable nil
        ;; lsp-ui-doc-max-width 200
        ;; lsp-ui-doc-max-height 30
        ;; lsp-signature-auto-activate nil
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil))

(use-package! company
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

;; TCP Server
(setq server-auth-key "N#'=2;T_VbOS#<,u~$bue@j1_C=n{/x'#'^vW532`5'OIYkRWGIUxWD.#]g$CC<U")
(setq server-host "0.0.0.0")
(setq server-port "8081")
(setq server-use-tcp t)

;; Rename buffer file
(defun my/rename-current-file ()
  "Rename the current visiting file and switch buffer focus to it."
  (interactive)

  (if (null (buffer-file-name))
      (user-error "Buffer does not have a filename: %s" (current-buffer)))
  (let ((new-filename (my/expand-filename-prompt
    		       (format "Rename %s to: " (file-name-nondirectory (buffer-file-name))))))
    (if (null (file-writable-p new-filename))
    	(user-error "New file not writable: %s" new-filename))

    (rename-file (buffer-file-name) new-filename 1)
    (find-alternate-file new-filename)
    (message "Renamed to and now visiting: %s" (abbreviate-file-name new-filename))))

(defun my/expand-filename-prompt (prompt)
  "Return expanded filename prompt."
  (expand-file-name (read-file-name prompt)))

(defalias '/rename 'my/rename-current-file)

(defun fun/make-latest-version-of-GT ()
  "Make latest version of GT."
  (interactive)
  (if (file-exists-p "/tmp/myGT")
      (async-shell-command "cd /tmp/myGT; cd */.; ./glamoroustoolkit")
      (async-shell-command "mkdir /tmp/myGT; cd /tmp/myGT; wget https://dl.feenk.com/gt/GlamorousToolkitLinux64-release.zip; unzip GlamorousToolkitLinux64-release.zip; cd */.; ./glamoroustoolkit")))

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
