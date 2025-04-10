;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(package! dired-plus)
(package! beacon)
(package! json-mode)
(package! json-reformat)
(package! lsp-ui)
(package! lsp-ivy)
(package! benchmark-init)
(package! magit-section)

;; Part of lang/org module
(package! org-fancy-priorities :disable t)
(package! elfeed-goodies :disable t)
(package! code-review :disable t)
;;(package! org-modern)
(package! deadgrep)
(package! org-modern-indent :recipe (:host github :repo "jdtsmith/org-modern-indent"))
(package! dired-du)
(package! clojure-ts-mode)
;;(package! elfeed-curate)
(unpin! (:lang clojure))
(package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
;;(package! password-menu)
(package! casual)
(unpin! elfeed elfeed-org)
(package! jinx)
(package! literate-calc-mode)
(package! howm)
(package! calfw)
(package! calfw-howm)
(package! android-mode :disable t)
(package! typespec-ts-mode)
(load! "./my-funcs.el")
(if (my/is-k8s-machine)
    (package! kubernetes)
  (unpin! (:tools magit)))
(unpin! (:tools magit))
(package! dirvish :disable t)
;; https://github.com/doomemacs/doomemacs/issues/8286
(package! package-lint :pin "21edc6d0d0eadd2d0a537f422fb9b7b8a3ae6991")
