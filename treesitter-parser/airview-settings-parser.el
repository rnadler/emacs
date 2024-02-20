
;; Use treesitter to parse Java source code and extract field names
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-Matching.html

(require 'cl-lib)

(setq field-query
      '((variable_declarator (identifier) @field-name)))

(setq mod-query
      '((modifiers (annotation) @ann-name
         (marker_annotation) @m-ann-name)))

(defun java-field-name (nodes)
  (cl-dolist (node nodes)
    (let ((qres (treesit-query-capture node field-query)))
      (when (not (null qres))
        (let ((n (cdr (assoc 'field-name qres))))
          (progn
            (message "field: %s mods=%s" (treesit-node-text n)
                     (prin1-to-string (treesit-query-capture (treesit-node-parent node) mod-query)))
            ;; (message "field: %s sibs=%s" (treesit-node-text n)
            ;;          (prin1-to-string (treesit-node-children (treesit-node-parent node))))
            (cl-return))
          )))
    )
  )

(let* ((parser (treesit-parser-create 'java (get-buffer "FlowGenSettings.java")))
       (root-node (treesit-parser-root-node parser))
       ;; Field name searching does not work...
       (class-body (treesit-node-child (treesit-node-child root-node -1) -1)))
  ;;(message (buffer-file-name (treesit-parser-buffer parser)))
  ;;(treesit-node-child-by-field-name root-node "class_declaration")
  (message "class child count = %d" (treesit-node-child-count class-body))
  (cl-dolist (child (treesit-node-children class-body))
    (java-field-name (treesit-node-children child))
    )
  )
