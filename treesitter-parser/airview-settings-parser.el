
;; Use treesitter to parse Java source code and extract field names and properties
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-Matching.html

(require 'cl-lib)

(setq field-query
      '((variable_declarator (identifier) @field-name)))

(setq type-query
      '((field_declaration (type_identifier) @type-name)))

(setq mod-query
      '((modifiers (annotation) @ann-name)))

(defun query-val (qres key)
  (cdr (assoc key qres)))

(defun java-field-name (nodes)
  (cl-dolist (node nodes)
    (let ((qres (treesit-query-capture node field-query)))
      (when (not (null qres))
        (let* ((n (query-val qres 'field-name))
               (parent (treesit-node-parent node))
               (type (query-val (treesit-query-capture parent type-query) 'type-name))
               (mods (treesit-query-capture parent mod-query)))
          (progn
            (message "field: %s type=%s mods(%d)=%s"
                     (treesit-node-text n)
                     (treesit-node-text type)
                     (length mods)
                     (mapconcat (lambda (mod) (treesit-node-text (cdr mod))) mods ", "))
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

(setq java-classes-dir "./flowGenClasses/")

(setq java-files
      '(
       ("FlowGenSettings.java" . 'flow_gen_settings)
       ("FlowGenVentilatorSettings.java" . 'flow_gen_ventilatory_settings)
       ("FlowGenSummaryDataProperties.java" . 'flow_gen_summary_data)
       ("FlowGenClimateSummaryDataProperties.java" . 'flow_gen_summary_data)
      ))

(defun open-file-buffer (fpath file-name)
  (when (null (get-buffer file-name))
    (find-file-noselect (concat fpath file-name))))

(dolist (f java-files) (open-file-buffer java-classes-dir (car f)))

(setq settings-java-file (car (car java-files)))
(setq java-file (car (nth 2 java-files))) ;; summary data

(let ((buffer (get-buffer java-file)))
  (if (null buffer)
      (message "%s buffer not found!" java-file)
    (parse-buffer buffer)))
