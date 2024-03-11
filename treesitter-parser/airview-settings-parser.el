
;; Use treesitter to parse Java source code and extract field names and properties

(require 'cl-lib)
(require 'json)

(setq field-query
      '((variable_declarator (identifier) @field-name)))

(setq type-query
      '((field_declaration (type_identifier) @type-name)))

(setq mod-query
      '((modifiers (annotation) @ann-name)))

(defun query-val (qres key)
  (cdr (assoc key qres)))

(defun to-snakecase (str)
  "Convert camelCase string STR to snake_case."
  (mapconcat #'(lambda (char)
                 (if (eq (upcase char) char)
                     (concat "_" (downcase (char-to-string char)))
                   (char-to-string char)))
             (string-to-list str) ""))

;; (to-snakecase "cheyneStokesRespirationMinutes")

(defun ecp-values (mods-str)
  "Get the ECP values"
  (let ((string mods-str)
        (regex "value = \"\\(.*\\)\",")
        (rv '()))
    (while (string-match regex string)
      (add-to-list 'rv (substring (match-string 1 string) 0 3))
      (setq string (substring string (match-end 0))))
    rv))

(defun remove-char-from-string (char string)
  "Remove CHAR from STRING."
  (replace-regexp-in-string (regexp-quote char) "" string))

(defun  ncp-suffixes(mods-str)
  "Get the NCP suffixes from MODS-STR"
  (let ((regex "'.*Val\\.\\(.*\\)'")
        (string mods-str)
        (pos 0)        ; string marker
        (matches ()))  ; return list
    (while (string-match regex string pos)
      (push (remove-char-from-string "'" (match-string 0 string)) matches)
      (setq pos (match-end 0)))
    (setq matches (reverse matches))
    matches))

;; (ncp-suffixes "@Enumerated(STRING), @JsonPathProperty(path = \"$.['iVAPS.Set.AutoEPAP', 'iVAPS.Val.AutoEPAP', 'Val.AutoEPAP']\"),")
;; (ncp-suffixes "@JsonPathProperty(path = \"$.['iVAPS.Set.MaxPS', 'iVAPS.Val.MaxPS', 'Val.iVAPS.MaxPS']\"),")
;; (ncp-suffixes "@Enumerated(STRING), @JsonPathProperty(path = \"$.['AutoSet.Set.Comfort', 'AutoSet.Val.Comfort']\"),")
;; (ncp-suffixes "@JsonPathProperty(path = \"$.['Val.Leak.50', 'Val.LeakVented.50']\"),")
;; (ncp-suffixes "@JsonPathProperty(path = \"$.['Val.RespRate.5']\"),")
;; (ncp-suffixes "@JsonPathProperty(path = \"$.['Val.Leak.Max']\"),")


(defun field-to-column (field mods-str)
  "Get the @Column name attribute from MODS-STR or the snakecase FIELD name"
  (let ((regex "^.*name = \"\\(.*\\)\".*$"))
    (if (string-match regex mods-str)
        (match-string 1 mods-str)
      (to-snakecase field))))

;; (field-to-column "defaultName" " @column(name = \"snake_case_name\")")

(defun java-field-name (nodes)
  (cl-dolist (node nodes)
    (let ((qres (treesit-query-capture node field-query)))
      (when (not (null qres))
        (let* ((n (query-val qres 'field-name))
               (parent (treesit-node-parent node))
               (type (query-val (treesit-query-capture parent type-query) 'type-name))
               (mods (treesit-query-capture parent mod-query))
               (field (treesit-node-text n))
               (mods-str (mapconcat (lambda (mod) (treesit-node-text (cdr mod))) mods ", "))
               (ncp (ncp-suffixes mods-str))
               (ecp (ecp-values mods-str)))
            (cl-return (if (or ecp ncp)
                           (list
                            (cons 'field field)
                            (cons 'type (treesit-node-text type))
                            (cons 'column (field-to-column field mods-str))
                            (cons 'ncp ncp)
                            (cons 'ecp ecp))
                         nil))
          )))))

(defun parse-class (class-nodes)
  (message "class child count = %d" (treesit-node-child-count class-nodes))
  (mapcar (lambda (child) (java-field-name (treesit-node-children child))) (treesit-node-children class-nodes)))

(defun parse-buffer (buffer)
  (let* ((parser (treesit-parser-create 'java buffer))
         (root-node (treesit-parser-root-node parser))
         (class-nodes (treesit-search-subtree root-node "class_body")))
    (parse-class class-nodes)))

(defun write-json-to-file (data file)
  "Write DATA (a JSON object) to FILE."
  (with-temp-buffer
    (insert (json-encode data))
    (json-reformat-region (point-min) (point-max))
    (write-region (point-min) (point-max) file)))

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

(setq java-file (car (car java-files))) ;; settings
(setq summary-java-file (car (nth 2 java-files))) ;; summary data
(setq vent-java-file (car (nth 1 java-files))) ;; vent data

(let ((buffer (get-buffer java-file)))
  (if (null buffer)
      (message "%s buffer not found!" java-file)
    (write-json-to-file (delq nil (parse-buffer buffer)) "test.json")
    ))
