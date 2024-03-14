
;; Use Emacs treesitter to parse Java source code and extract ECO database field names and properties
;; Usage: M-x eval-buffer

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
                 (let ((s (char-to-string char)))
                   (if (and (not (string-match-p "[0-9]" s)) (eq (upcase char) char))
                       (concat "_" (downcase s))
                     s)))
             (string-to-list str) ""))

;; (to-snakecase "cheyneStokesRespirationMinutes")
;; (to-snakecase "expiratoryPressure95thPercentile")

(defun ncp-unique (ncp-list)
  "Return a list of unique NCP name elements for NCP-LIST."
  (let ((ncp-unique ()))
    (dolist (ncp ncp-list)
      (dolist (suffix (reverse (split-string ncp "\\.")))
        (unless (member suffix ncp-unique)
          (push suffix ncp-unique))))
    ncp-unique))

(defun ncp-unique-list (ncp-list)
  "List of unique NCP VAL/SET names from NCP-LIST."
  (let ((selected '("Set" "Val"))
        (lst (reverse (ncp-unique ncp-list)))
        (rv ()))
    (cl-dolist (x lst)
      (if (member x selected)
          (let ((rstr (mapconcat #'identity rv "."))
                (sel (seq-filter (lambda (v) (member v lst)) selected)))
            (cl-return (mapcar (lambda (y) (concat y "." rstr)) sel))))
          (push x rv)))
    )

;; (ncp-unique-list '("Val.AI"))
;; (ncp-unique-list '("iVAPS.Set.AutoEPAP" "iVAPS.Val.AutoEPAP" "Val.AutoEPAP"))
;; (ncp-unique-list '("iVAPS.Set.MaxPS" "iVAPS.Val.MaxPS" "Val.iVAPS.MaxPS"))
;; (ncp-unique-list '("CPAP.Set.Ramp.RampEnable" "CPAP.Val.Ramp.RampEnable" "AutoSet.Set.Ramp.RampEnable" "AutoSet.Val.Ramp.RampEnable"))

(defun find-values (regex string)
  "Return all matches of REGEX in STRING."
  (let ((matches ()))
    (while (string-match regex string)
      (push (match-string 1 string) matches)
      (setq string (substring string (match-end 0))))
    (reverse matches)))

(defun ecp-values (mods-str)
  "Get the ECP values"
  (find-values "value = \"\\(.*?\\)\"" mods-str))

;; (ecp-values "@AcronymMap(value = \"AFC\", clazz = SettingsComfortMode.class, method = \"fromSettingsComfortMode\")")

(defun  ncp-suffixes(mods-str)
  "Get the NCP suffixes from MODS-STR"
  (ncp-unique-list (find-values "'\\(.*?\\)'" mods-str)))

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

(defun field-list (field type column ncp ecp &optional table)
  "Return a list of field attributes"
  (let ((rv (list (cons 'field field)
                  (cons 'type type)
                  (cons 'column column)
                  (cons 'ncp ncp)
                  (cons 'ecp ecp))))
    (when table (push table rv))
    rv))

(defun java-field-name (nodes)
  "Return a list of field attribute from NODES.
   Return nil if no NCP or ECP is found."
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
                           (field-list field (treesit-node-text type)
                                       (field-to-column field mods-str) ncp ecp)
                         nil)))))))

(defun parse-class (class-nodes)
    (mapcar (lambda (child) (java-field-name (treesit-node-children child)))
            (treesit-node-children class-nodes)))

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

(defun open-file-buffer (fpath file-name)
  (when (null (get-buffer file-name))
    (find-file-noselect (concat fpath file-name))))

(defun process-java-file (java-file)
  (let* ((file-name (car java-file))
         (_ (open-file-buffer java-classes-dir file-name))
         (buffer (get-buffer file-name)))
    (if (null buffer)
        (message "%s buffer not found!" file-name)
      (delq nil (parse-buffer buffer)))
    ))

(defun table-field (table-name)
  "Return a field attribute for TABLE-NAME."
  (cons 'table table-name))

;; ------------------------------------------------------------
;; Constants

(setq missing-fields
      (list (field-list "durationMinutes" "Integer" "duration_minutes" '("Val.Duration") '("OND")
                        (table-field "flow_gen_summary_data"))))


(setq java-classes-dir "./flowGenClasses/")
(setq output-file "eco-db-data.json")

(setq java-files
      '(
       ("FlowGenSettings.java" .                     "flow_gen_settings")
       ("FlowGenVentilatorSettings.java" .           "flow_gen_ventilator_settings")
       ("FlowGenSummaryDataProperties.java" .        "flow_gen_summary_data")
       ("FlowGenClimateSummaryDataProperties.java" . "flow_gen_summary_data")
      ))

;; ------------------------------------------------------------
;; Execute the parser

(defun process-all-java-files ()
  (let ((data missing-fields))
    (dolist (f java-files)
      (let ((table-name (table-field (cdr f)))
            (d (process-java-file f)))
        (setq d (mapcar (lambda (e) (push table-name e)) d))
        (setq data (append data d))))
    (message "Wrote %d records to %s" (length data) output-file)
    (write-json-to-file data output-file)))

(process-all-java-files)
