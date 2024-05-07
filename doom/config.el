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
;; (setq doom-font (font-spec :family "Ubuntu Mono" :size 40)
;;       doom-variable-pitch-font (font-spec :family "Ubuntu" :size 40)
;;       doom-big-font (font-spec :family "Ubuntu Mono" :size 52))

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

;;(display-time)
(setq org-hide-emphasis-markers t)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq confirm-kill-processes nil)
(setq pixel-scroll-precision-mode t)  ;; New Emacs 29 feature

(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x l") 'display-line-numbers-mode)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

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

;; https://zck.org/improved-emacs-search
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

;;(setq auth-source-debug t)
(after! magit
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source))

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
;; $ ncftpput -R -v -u "rdn-cons" ftp.rdn-consulting.com /public_html ./coi

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
  (use-package! elfeed-webkit :bind (:map elfeed-show-mode-map ("x" . elfeed-webkit-toggle)))
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

;; elfeed-curate (WIP)
(load! "~/Projects/elfeed-curate/elfeed-curate.el")

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
  (setq all-the-icons-dired-monochrome nil)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(setq diredp-hide-details-initially-flag nil)
(diredp-toggle-find-file-reuse-dir 1)
;; https://www.emacs.dyerdwelling.family/emacs/emacs--dired-going-up-directories__emacs_linux/
(define-key dired-mode-map (kbd "M-u") 'dired-up-directory)
(fset 'file-up-dir (kmacro [?\C-x ?d return ] 0 "%d"))
(global-set-key (kbd "M-u") 'file-up-dir)

;; https://github.com/kickingvegas/casual-dired
(define-key dired-mode-map (kbd "C-o") 'casual-dired-tmenu)
(after! casual-dired
  (setq casual-dired-use-utf8-symbols t))

(defun my/disable-line-numbers (&optional _)
  (display-line-numbers-mode -1))

;; Set initial frame size and position

(defun my/frame-recenter (&optional frame)
"Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

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
  (setq org-execute-file-search-functions nil
        org-agenda-current-time-string "⬅ now ─────────────────────────────────────────────────")
  (org-link-set-parameters  "copy"
                            :follow (lambda (link) (kill-new link))
                            :export (lambda (_ desc &rest _) desc)))
(after! org-modern
  ;; (setq org-modern-hide-stars nil) ; adds extra indentation
  ;; (setq org-modern-table nil)
  (setq org-modern-list
   '((?* . "•")
     (?+ . "‣")))
  (setq org-modern-star 'replace)
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
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

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

;; https://www.emacs.dyerdwelling.family/emacs/20240305160708-emacs--unified-interface-for-switching-contexts/
;; 05-Mar-2024
(defun my/switch-to-thing ()
  "Switch to a buffer, open a recent file, jump to a bookmark, or change the theme from a unified interface."
  (interactive)
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (recent-files recentf-list)
         (bookmarks (bookmark-all-names))
         (themes (custom-available-themes))
         (all-options (append buffers recent-files bookmarks
                              (mapcar (lambda (theme) (concat "Theme: " (symbol-name theme))) themes)))
         (selection (completing-read "Switch to: "
                                     (lambda (str pred action)
                                       (if (eq action 'metadata)
                                           '(metadata . ((category . file)))
                                         (complete-with-action action all-options str pred)))
                                     nil t nil 'file-name-history)))
    (pcase selection
      ((pred (lambda (sel) (member sel buffers))) (switch-to-buffer selection))
      ((pred (lambda (sel) (member sel bookmarks))) (bookmark-jump selection))
      ((pred (lambda (sel) (string-prefix-p "Theme: " sel)))
       (load-theme (intern (substring selection (length "Theme: "))) t))
      (_ (find-file selection)))))
(global-set-key (kbd "C-x t") 'my/switch-to-thing)

;; Password Menu
;;

(load! "~/Projects/password-menu/password-menu.el")

(global-set-key (kbd "C-x j") 'password-menu-transient)
(global-set-key (kbd "C-x J") 'password-menu-completing-read)

(setq auth-sources '("~/.authinfo.gpg"))

;; WSL specific stuff
;; https://emacsredux.com/blog/2021/12/19/wsl-specific-emacs-configuration/
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  ;; Teach Emacs how to open links in your default Windows browser
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic)))
  ;; https://emacsredux.com/blog/2021/12/19/using-emacs-on-windows-11-with-wsl2/
  (defun my/copy-selected-text (start end)
    (interactive "r")
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties start end)))
          (shell-command (concat "echo '" text "' | clip.exe"))))))

;; Org-roam v1
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

;; Org link to dired

(defun org-open-file-with-dired (path)
  "Open in dired."
  (let ((d (file-name-directory path))
        (f (file-name-nondirectory path)))
    (dired d)
    (goto-char (point-min))
    (search-forward f nil t)))

(org-link-set-parameters "dired" :follow 'org-open-file-with-dired)

;; org-roam-ui (for org-roam v2)
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; Clojure
;; https://github.com/ericdallo/dotfiles/blob/master/.doom.d/config.el#L99-L134
(use-package! cider
  :after clojure-mode
  :config
  (setq cider-ns-refresh-show-log-buffer t
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

;; LSP
(after! lsp-mode
  ;; :commands lsp
  ;; :hook ((clojure-mode . lsp)
  ;;        (clojurescript-mode . lsp)
  ;;        (lsp-mode . lsp-enable-which-key-integration)
  ;;        ;;(java-mode . lsp)
  ;;        )
  :config
  (setq ;; clojure-lsp is on PATH
        ;; lsp-clojure-custom-server-command '("bash" "-c" "/usr/bin/clojure-lsp")
        lsp-headerline-breadcrumb-enable t
        ;; lsp-lens-enable t
        ;; lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        lsp-signature-render-documentation t
        lsp-signature-auto-activate t
        lsp-completion-use-last-result nil))

;; (use-package! lsp-ivy
;;   :commands lsp-ivy-workspace-symbol)

(after! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-list-width 60
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'top
        lsp-ui-doc-max-width 300
        lsp-ui-doc-max-height 30
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil))

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
  :init
  (global-corfu-mode))

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

;; https://www.emacs.dyerdwelling.family/emacs/20231013153639-emacs--more-flexible-duplicate-thing-function/
(defun my/dired-duplicate-file (arg)
  "Duplicate a file from dired with an incremented number.
If ARG is provided, it sets the counter."
  (interactive "p")
  (let* ((file (dired-get-file-for-visit))
          (dir (file-name-directory file))
          (name (file-name-nondirectory file))
          (base-name (file-name-sans-extension name))
          (extension (file-name-extension name t))
          (counter (if arg (prefix-numeric-value arg) 1))
          (new-file))
    (while (and (setq new-file
                  (format "%s%s_%03d%s" dir base-name counter extension))
             (file-exists-p new-file))
      (setq counter (1+ counter)))
    (if (file-directory-p file)
      (copy-directory file new-file)
      (copy-file file new-file))
    (dired-revert)))

;; The key binding doesn't seem to work...
;;(define-key dired-mode-map (kbd "C-c d") 'my/dired-duplicate-file)

(defun my/expand-filename-prompt (prompt)
  "Return expanded filename prompt."
  (expand-file-name (read-file-name prompt)))

(defalias '/rename 'my/rename-current-file)

(defun my/make-latest-version-of-GT ()
  "Make latest version of GT."
  (interactive)
  (if (file-exists-p "/tmp/myGT")
      (async-shell-command "cd /tmp/myGT; cd */.; ./glamoroustoolkit")
      (async-shell-command "mkdir /tmp/myGT; cd /tmp/myGT; wget https://dl.feenk.com/gt/GlamorousToolkitLinux64-release.zip; unzip GlamorousToolkitLinux64-release.zip; cd */.; ./glamoroustoolkit")))

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
        ("~/Projects/eco-builds" . 1)
        ("~/Projects/eco-builds-config" . 1)
        ("~/Projects/eco-builds-ui" . 1)
        ("~/Projects/variant-json" . 1)))

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
