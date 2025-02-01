;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "./my-funcs.el")

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
(if (my/is-wsl)
    (setq font-size [16 16 24])
    (setq font-size [14 14 20]))

 (setq doom-font (font-spec :family "Ubuntu Mono" :size (aref font-size 0))
       doom-variable-pitch-font (font-spec :family "Ubuntu" :size (aref font-size 1))
       doom-big-font (font-spec :family "Ubuntu Mono" :size (aref font-size 2)))

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
(global-visual-line-mode +1)
(setq visible-bell t)
(global-unset-key (kbd "C-z"))
(winner-mode +1)

(setq org-hide-emphasis-markers t)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq confirm-kill-processes nil)
(setq pixel-scroll-precision-mode t)  ;; New Emacs 29 feature

(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x l") 'display-line-numbers-mode)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

;; https://zck.org/improved-emacs-search
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

;;(setq auth-source-debug t)
(after! magit
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source)
  (setq magit-show-long-lines-warning nil))

;; Copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; Elfeed
;; https://github.com/skeeto/elfeed/issues/292
(eval-when-compile (require 'elfeed))
(global-set-key (kbd "C-x w") 'elfeed)

(defun my/elfeed-clean-title (title)
  "Clean string TITLE."
  (->> title
       (string-replace "<b>" "")
       (string-replace "</b>" "")
       (string-replace "<sub>" "")
       (string-replace "</sub>" "")
       (string-replace "<i>" "")
       (string-replace "</i>" "")
       (string-replace "&#xa0;" " ")
       (string-replace "&#39;" "'")
       (string-replace "&amp;" "&")
       (string-replace "&lt;" "<")
       (string-replace "&gt;" ">")
       (string-replace "&quot;" "\"")))

;; (my/elfeed-clean-title "<b>USA&#39;s</b> Merative &amp; That &lt;code&gt; &quot;Quoted&quot;")
;; (setq elfeed-curate-org-export-backend 'html)

;; (with-current-buffer (elfeed-search-buffer)
;;   (let* ((groups (elfeed-curate-group-org-entries elfeed-search-entries))
;;         (group-keys (elfeed-curate-plist-keys groups)))
;;     (with-current-buffer (generate-new-buffer "*curate-temp*")
;;       (mapc (lambda (group-key) (elfeed-curate-add-org-group group-key (plist-get groups group-key) t)) group-keys))
;;     ;;(elfeed-curate--group-entries-count groups)
;;     ))

;; Hugo deploy:
;; $ sudo apt-get install ncftp
;; $ hugo
;; $ ncftpput -R -v -u "hold67" ftp.rdn-consulting.com /public_html ./coi

(defun my/elfeed-clean-entry (entry)
  "Clean the title of an ENTRY"
  (let ((title (elfeed-entry-title entry)))
      (setf (elfeed-entry-title entry)
            (my/elfeed-clean-title title))))

(after! elfeed
  (setq-default elfeed-search-filter "+unread")
  (setq elfeed-sort-order 'ascending)
  (advice-add #'elfeed-insert-html
              :around
              (lambda (fun &rest r)
                (let ((shr-use-fonts nil))
                  (apply fun r))))
  ;; From http://pragmaticemacs.com/emacs/star-and-unstar-articles-in-elfeed/
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))
  ;; (eval-after-load 'elfeed-search
  ;;   '(define-key elfeed-search-mode-map (kbd "m") 'elfeed-toggle-star))
  (define-key elfeed-search-mode-map "m" #'elfeed-toggle-star)
  ;; face for starred articles
  (defface elfeed-search-star-title-face '((t :foreground "#f77")) "Marks a starred Elfeed entry.")
  (push '(star elfeed-search-star-title-face) elfeed-search-face-alist)
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))
  ;; elfeed-curate key bindings
  (define-key elfeed-search-mode-map "a" #'elfeed-curate-edit-entry-annoation)
  (define-key elfeed-search-mode-map "x" #'elfeed-curate-export-entries)
  ;;(define-key elfeed-search-mode-map "m" #'elfeed-curate-toggle-star)  ;; like the toggle-all better
  (define-key elfeed-show-mode-map "a" #'elfeed-curate-edit-entry-annoation)
  (define-key elfeed-show-mode-map "m" #'elfeed-curate-toggle-star)
  (define-key elfeed-show-mode-map "q" #'kill-buffer-and-window)
  ;;(add-hook 'elfeed-tag-hooks (lambda (entry tag) (elfeed-curate--show-entry "Add tag" (car entry) tag)))
  ;;(add-hook 'elfeed-untag-hooks  (lambda (entry tag) (elfeed-curate--show-entry   "Remove tag" (car entry) tag)))

  (add-hook 'elfeed-new-entry-hook #'my/elfeed-clean-entry)
  ;;(setq elfeed-curate-org-export-backend 'md)
  (setq elfeed-curate-hugo-base-dir "~/Projects/content-of-interest/")
)

;; elfeed-curate
(load! "~/Projects/elfeed-curate/elfeed-curate.el")

;; Config diffs
;; (my/diff-config "init.el")
;; (my/diff-config "config.el")
;; (my/diff-config "packages.el")
;; (my/diff-config "org-config.el")
;; (my/diff-config "my-funcs.el")

(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))

(add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.tsp\\'" . typespec-ts-mode))


(when (not (my/is-k8s-machine))
  (display-time))

;; Kubernetes
(when (my/is-k8s-machine)
  (after! kubernetes
    :ensure t
    :commands (kubernetes-overview)
    :config
    (setq kubernetes-poll-frequency 3600
          kubernetes-redraw-frequency 3600))
  (fset 'k8s 'kubernetes-overview))

(setq make-backup-files nil) ;; stop creating those backup~ files

(setq-default dired-omit-files-p t) ; Buffer-local variable
(after! dired+
  (setq all-the-icons-dired-monochrome nil)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(after! dirvish
  (setq dirvish-attributes '(nerd-icons collapse)))

(setq diredp-hide-details-initially-flag nil)
(diredp-toggle-find-file-reuse-dir 1)
;; https://www.emacs.dyerdwelling.family/emacs/emacs--dired-going-up-directories__emacs_linux/
(define-key dired-mode-map (kbd "M-u") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c m") 'my/dired-meld-diff-all-dwim)
(fset 'file-up-dir (kmacro [?\C-x ?d return ] 0 "%d"))
(global-set-key (kbd "M-u") 'file-up-dir)

;; Execute from the *elfeed-search* buffer
(defalias 'elfeed-backup
   (kmacro "C-x k RET C-c b C-x d / h o m e / b o b n / P r o j e c t s / c o n t e n t - o f - i n t e r e s t RET M-< C-s g e t _ RET ! RET g"))


;; https://github.com/kickingvegas/casual-dired
(setq casual-lib-use-unicode t)
(use-package! casual-dired
  :bind (:map
         dired-mode-map ("C-o" . casual-dired-tmenu)))
;; https://github.com/kickingvegas/casual-agenda
(use-package! casual-agenda
  :bind (:map
         org-agenda-mode-map ("C-o" . casual-agenda-tmenu))
  :after (org-agenda))
;; https://github.com/kickingvegas/casual-editkit
(use-package! casual-editkit
  :bind (("C-o" . casual-editkit-main-tmenu)))

;; [2024-11-26 Tue] -- Doesn't work with =C-c !=
;; https://github.com/kickingvegas/casual/blob/main/docs/calendar.org
(keymap-set  calendar-mode-map "C-o" #'casual-calendar)

(defun my/org-minibuffer-keymap-setup ()
  (let ((map (current-local-map)))
    (when map
      (define-key map (kbd "C-o") 'casual-calendar))))

;; (defun my/add-minibuffer-setup-hook (&rest _args)
;;   (add-hook 'minibuffer-setup-hook #'my/org-minibuffer-keymap-setup))

;; (defun my/remove-minibuffer-setup-hook (&rest _args)
;;   (remove-hook 'minibuffer-setup-hook #'my/org-minibuffer-keymap-setup))

;; (defun my/setup-minibuffer-for-func (func)
;;   (advice-add func :before #'my/add-minibuffer-setup-hook)
;;   (advice-add func :after #'my/remove-minibuffer-setup-hook))

;; (my/setup-minibuffer-for-func #'org-deadline)
;; (my/setup-minibuffer-for-func #'org-schedule)
;; (my/setup-minibuffer-for-func #'org-time-stamp-inactive)

;; Wrapper functions
(defun my/org-deadline ()
  (interactive)
  (minibuffer-with-setup-hook 'my/org-minibuffer-keymap-setup
    (call-interactively 'org-deadline)))

(defun my/org-schedule ()
  (interactive)
  (minibuffer-with-setup-hook 'my/org-minibuffer-keymap-setup
    (call-interactively 'org-schedule)))

(defun my/org-time-stamp-inactive ()
  (interactive)
  (minibuffer-with-setup-hook 'my/org-minibuffer-keymap-setup
    (call-interactively 'org-time-stamp-inactive)))

(global-set-key (kbd "C-c C-d") 'my/org-deadline)
(global-set-key (kbd "C-c C-s") 'my/org-schedule)
(global-set-key (kbd "C-c !") 'my/org-time-stamp-inactive)


(setq frame-resize-pixelwise t)
(my/set-initial-frame)
(setq confirm-kill-emacs nil)
(grep-apply-setting 'grep-find-command '("find . -name '*.java' -exec grep -nH --null -e   \\{\\} +" . 48))
;; Org mode
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(after! org
  (load! "./org-config.el")
  (setq org-execute-file-search-functions nil
        org-agenda-current-time-string "⬅ now ─────────────────────────────────────────────────")
  (org-link-set-parameters  "copy"
                            :follow (lambda (link) (kill-new link))
                            :export (lambda (_ desc &rest _) desc)))
(after! org-modern
  (setq org-modern-star 'replace)
  (setq org-modern-checkbox
        '((?X . "✅")    ;; Completed checkbox
          (?- . "🔆")    ;; Pending checkbox
          (?\s . "🔳"))) ;; Empty checkbox
  (setq org-modern-todo-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)))))
(global-org-modern-mode)
(add-hook 'org-mode-hook #'org-modern-indent-mode 90)
;; https://discord.com/channels/406534637242810369/1019657860361224202/threads/1085222477904498719
;; Enable compact on search buffer delete
(advice-add #'+rss-cleanup-h :after #'+rss--cleanup-on-kill-h)
;; Disable compact on emacs exit
(defadvice! cleanup-on-kill (fn &rest args)
  :around #'+rss-cleanup-h
  (let (kill-emacs-hook)
    (prog1 (apply fn args)
      (+rss--cleanup-on-kill-h))))

;; Beacon
(beacon-mode 1)

;; Enchant/jinx
;; sudo apt-get install libenchant-2-dev pkgconf
(add-hook 'emacs-startup-hook #'global-jinx-mode)
(keymap-global-set "M-$" #'jinx-correct)
(keymap-global-set "C-M-$" #'jinx-languages)

;; Treesitter support
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (typespec "https://github.com/happenslol/tree-sitter-typespec")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (clojure-mode . clojure-ts-mode)
   (clojurescript-mode . clojure-ts-clojurescript-mode)
   ;; (emacs-lisp-mode . emacs-lisp-ts-mode)
   ;; (web-mode . html-ts-mode)
   (dockerfile-mode . dockerfile-ts-mode)
   (sh-mode . bash-ts-mode)
   (ruby-mode . ruby-ts-mode)
   (js2-mode . js-ts-mode)
   (rjsx-mode . js-ts-mode)
   (java-mode . java-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (typescript-tsx-mode . tsx-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

;; Python scons support
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

;; Password Menu
;;

(load! "~/Projects/password-menu/password-menu.el")

(global-set-key (kbd "C-x j") 'password-menu-transient)
(global-set-key (kbd "C-x J") 'password-menu-completing-read)

(setq auth-sources '("~/.authinfo.gpg"))

;; WSL specific stuff
;; https://emacsredux.com/blog/2021/12/19/wsl-specific-emacs-configuration/
;;

(when (my/is-wsl)
  ;; Teach Emacs how to open links in your default Windows browser
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic)))
  (defun my/wsl-copy-to-clipboard (text &optional push)
    "Copy TEXT to the Windows clipboard using clip.exe."
    (let ((process-connection-type nil))
      (let ((proc (start-process "clip" "*Messages*" "clip.exe")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'my/wsl-copy-to-clipboard)
)

(org-link-set-parameters "dired" :follow 'my/org-open-file-with-dired)

;; Howm

(use-package! howm
  :init
  (setq howm-directory (expand-file-name "howm" my/org-directory))
  (setq howm-home-directory howm-directory)
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
  (setq howm-file-name-format "%Y-%m-%d-%H%M%S.org")
  (setq howm-view-title-header "*")
  (setq action-lock-switch-default '("{ }" "{○}" "{◔}" "{◑}" "{◕}" "{●}"))
  (setq howm-dtime-body-format "%Y-%m-%d %a %H:%M")
  (setq howm-prefix (kbd "C-c ;"))
  (setq howm-menu-recent-num 10)
  (custom-set-faces
   '(howm-reminder-normal-face ((t (:foreground "gray" :background "MidnightBlue" :weight bold)))))
  ;; Use ripgrep
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)
  (add-hook 'howm-view-summary-mode-hook 'my/disable-line-numbers)
  :bind*
  ("C-c ; ;" . howm-menu))
  (after! howm
    (let ((file-name "0000-00-00-000000"))
      (when (not (string-match-p file-name howm-excluded-file-regexp))
        (setq howm-excluded-file-regexp  (concat howm-excluded-file-regexp
                                                 (concat "\\|" file-name "\\.txt")))))
    (require 'calfw-howm)
    (cfw:install-howm-schedules)
    ;; Add to howm-menu (0000-00-00-000000.txt):
    ;;   %here%(cfw:howm-schedule-inline)
    (define-key howm-mode-map (kbd "M-C") 'cfw:open-howm-calendar))

;; Clojure
;; https://github.com/ericdallo/dotfiles/blob/master/.doom.d/config.el#L99-L134
(use-package! cider
  :after clojure-mode
  :config
  (Setq cider-ns-refresh-show-log-buffer t
        cider-show-error-buffer t;'only-in-repl
        cider-font-lock-dynamically '(macro core function var deprecated)
        cider-prompt-for-symbol nil)
  (set-lookup-handlers! 'cider-mode nil)
  (setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))"))

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

(after! clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-thread-all-but-last t
        clojure-align-forms-automatically t
        yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-c")
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode))

;; Eglot
;; npm install -g typescript-language-server typescript
;; npm install -g pyright
;; Install jdtls (Java language server)
;; $ wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
;; $ mkdir ~/Downloads/jdtls
;; $ tar -xvzf jdt-language-server-latest.tar.gz -C ~/Downloads/jdtls
;; $ sudo ln -s ~/Downloads/jdtls/bin/jdtls /usr/local/bin/jdtls

;; Lombok support
(setq lombok-version "1.18.34")
(setq lombok-jar-path (expand-file-name (format "~/.m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar"
                                                lombok-version lombok-version)))
(use-package! eglot
  :commands (eglot eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs `((clojure-mode clojurescript-ts-mode) . ("clojure-lsp")))
  (add-to-list 'eglot-server-programs
               `(java-mode . ("jdtls" ,(concat "--jvm-arg=-javaagent:" lombok-jar-path))))
  :hook ((js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (clojure-mode . eglot-ensure)
         (clojurescript-ts--mode . eglot-ensure)
         (java-mode . eglot-ensure)))

;; Smartparens
(after! smartparens
 (global-set-key (kbd "C-)") 'sp-forward-slurp-sexp)
 (global-set-key (kbd "C-(") 'sp-forward-barf-sexp)
 (global-set-key (kbd "C-{") 'sp-backward-unwrap-sexp)
 (global-set-key (kbd "C-}") 'sp-unwrap-sexp)
 (show-paren-mode 1))

;; corfu
(use-package! corfu
  :custom
  (corfu-scroll-margin 10)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  ;;(corfu-preselect 'prompt)
  ;;(corfu-on-exact-match nil)
    :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; cape
(after! cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; TCP Server
(setq server-auth-key "N#'=2;T_VbOS#<,u~$bue@j1_C=n{/x'#'^vW532`5'OIYkRWGIUxWD.#]g$CC<U")
(setq server-host "0.0.0.0")
(setq server-port "8081")
(setq server-use-tcp t)
(server-start)

;; Make Script Files Executable Automatically
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Magit repo status
(setq magit-repolist-columns
      '(("Name"    25 magit-repolist-column-ident ())
        ("Version" 25 magit-repolist-column-version ())
        ("D"        1 magit-repolist-column-dirty ())
        ("⇣"      3 magit-repolist-column-unpulled-from-upstream
         ((:right-align t)
          (:help-echo "Upstream changes not in branch")))
        ("⇡"        3 magit-repolist-column-unpushed-to-upstream
         ((:right-align t)
          (:help-echo "Local changes not in upstream")))
        ("Path"    99 magit-repolist-column-path ())))
(setq magit-repository-directories
      `(
        ("~/Projects/airview-web" . 1)
        ("~/Projects/exchange" . 1)
        ("~/Projects/airview-eco-builds" . 1)
        ("~/Projects/eco-builds-config" . 1)
        ("~/Projects/airview-eco-builds-web" . 1)
        ("~/Projects/airview-variant-json" . 1)))

;; magit-delta
;;(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; REBL: https://github.com/RobertARandolph/cider-rebl
;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; C-S-x send defun to rebl
;; C-x C-r send last sexp to rebl (Normally bound to "find-file-read-only"... Who actually uses that though?)
(add-hook 'cider-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-x") #'rebl-eval-defun-at-point)
            ;;(local-set-key (kbd "C-x C-r") #'rebl-eval-last-sexp)
            ))

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
