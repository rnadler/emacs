;;; ~/.doom.d/init-treemacs.el -*- lexical-binding: t; -*-

(setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
      treemacs-deferred-git-apply-delay      0.5
      treemacs-directory-name-transformer    #'identity
      treemacs-display-in-side-window        t
      treemacs-eldoc-display                 t
      treemacs-file-event-delay              5000
      treemacs-file-extension-regex          treemacs-last-period-regex-value
      treemacs-file-follow-delay             0.2
      treemacs-file-name-transformer         #'identity
      treemacs-follow-after-init             t
      treemacs-git-command-pipe              ""
      treemacs-goto-tag-strategy             'refetch-index
      treemacs-indentation                   2
      treemacs-indentation-string            " "
      treemacs-is-never-other-window         nil
      treemacs-max-git-entries               5000
      treemacs-missing-project-action        'ask
      treemacs-no-png-images                 nil
      treemacs-no-delete-other-windows       t
      treemacs-project-follow-cleanup        nil
      treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
      treemacs-position                      'left
      treemacs-recenter-distance             0.1
      treemacs-recenter-after-file-follow    nil
      treemacs-recenter-after-tag-follow     nil
      treemacs-recenter-after-project-jump   'always
      treemacs-recenter-after-project-expand 'on-distance
      treemacs-show-cursor                   nil
      treemacs-show-hidden-files             nil
      treemacs-silent-filewatch              nil
      treemacs-silent-refresh                nil
      treemacs-sorting                       'alphabetic-asc
      treemacs-space-between-root-nodes      t
      treemacs-tag-follow-cleanup            t
      treemacs-tag-follow-delay              1.5
      treemacs-width                         35)
(treemacs-follow-mode nil)
(treemacs-filewatch-mode t)
(treemacs-fringe-indicator-mode t)

(after! doom-themes
   (setq doom-themes-treemacs-theme "Default")
   (doom-themes-treemacs-config))

(after! treemacs-icons-dired dired
  (treemacs-icons-dired-mode))
