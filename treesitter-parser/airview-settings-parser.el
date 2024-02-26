
;; Use treesitter to parse Java source code and extract field names and properties
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-Matching.html

(require 'cl-lib)

(setq field-query
      '((variable_declarator (identifier) @field-name)))

(setq mod-query
      '((modifiers (annotation) @ann-name)))

(defun java-field-name (nodes)
  (cl-dolist (node nodes)
    (let ((qres (treesit-query-capture node field-query)))
      (when (not (null qres))
        (let ((n (cdr (assoc 'field-name qres))))
          (progn
            (message "field: %s mods=%s" (treesit-node-text n)
                     (mapconcat (lambda (mod) (treesit-node-text (cdr mod)))
                                (treesit-query-capture (treesit-node-parent node) mod-query) ", "))
            (cl-return))
          )))))

(defun parse-class (class-nodes)
  (message "class child count = %d" (treesit-node-child-count class-nodes))
  (cl-dolist (child (treesit-node-children class-nodes))
    (java-field-name (treesit-node-children child))))

(defun parse-buffer (buffer)
  (let* ((parser (treesit-parser-create 'java buffer))
         (root-node (treesit-parser-root-node parser))
         (class-nodes (treesit-search-subtree root-node "class_body")))
    (parse-class class-nodes)))

;; ------------------------------------------------------------
;; Execute the parser

(setq java-file
      "FlowGenSettings.java"
      ;; "FlowGenSummaryDataProperties.java"
      ;; "FlowGenClimateSummaryDataProperties.java"
      ;; "FlowGenVentilatorSettings.java"
      )

(let ((buffer (get-buffer java-file)))
  (if (null buffer)
      (message "%s buffer not found!" java-file)
    (parse-buffer buffer)))
