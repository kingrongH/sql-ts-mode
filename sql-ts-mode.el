;;; sql-ts-mode.el --- Major mode for editing SQL using tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Kingrong Huang <kingronghuang@gmail.com>
;; Maintainer: Kingrong Huang  <kingronghuang@gmail.com>
;; Created: 2025-03-31
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (treesit "0.3"))
;; Homepage: https://github.com/your/repo/location ;; Optional
;; Keywords: languages sql tools tree-sitter

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Provides sql-ts-mode, a major mode for editing SQL files using
;; the tree-sitter parsing library.  This mode provides syntax
;; highlighting based on the tree-sitter-sql grammar.

;;; Code:

(require 'treesit)

;; --- Syntax Table ---
;; Taken from `sql-mode'
(defvar sql-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C-style comments /**/ (see elisp manual "Syntax Flags"))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    ;; double-dash starts comments
    (modify-syntax-entry ?- ". 12b" table)
    ;; newline and formfeed end comments
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\f "> b" table)
    ;; single quotes (') delimit strings
    (modify-syntax-entry ?' "\"" table)
    ;; double quotes (") don't delimit strings
    (modify-syntax-entry ?\" "." table)
    ;; Make these all punctuation
    (mapc (lambda (c) (modify-syntax-entry c "." table))
          (string-to-list "!#$%&+,.:;<=>?@\\|"))
    table)
  "Syntax table used in `sql-ts-mode'.")


;; --- Indentation ---
;; Basic indentation offset. More complex rules can be added later using
;; `treesit-indent-rules'.
(defcustom sql-ts-mode-indent-offset 2
  "Basic indentation offset for `sql-ts-mode'."
  :type 'integer
  :group 'sql-ts)

;; --- Indentation Rules ---

(defun sql-ts-mode--parent-bol-until (type)
  "This anchor is a function of one argument 'TYPE'; It return a function that is called with 3 arguments: node, parent, and bol, and return non-nil (i.e., a match) if recursive parent’s type matches regexp type."
  (lambda (node _parent _bol)
    (treesit-node-start (treesit-parent-until
     node
     (lambda (node)
       (string-equal (treesit-node-type node) type))))))

(defun sql-ts-mode--prev-sibling-bol-util (type)
  "This anchor is a function of one argument 'TYPE'"
  (lambda (node _parent _bol)
    (let ((node (treesit-node-prev-sibling node)))
      (while (and node (not (string-equal (treesit-node-type node) type)))
	(setq node (treesit-node-prev-sibling node)))
      (treesit-node-start node))))

(defun sql-ts-mode--first-sibling-bol (_node parent _bol)
  "This anchor return a function represent first sibling bol(begin or line."
  (save-excursion
    (goto-char (treesit-node-start (treesit-node-child  parent 0)))
    (back-to-indentation)
    (point)))

(defvar sql-ts-mode--indent-rules
  `((sql ; Language Symbol
     ))
  "Tree-sitter indent rules for `sql-ts-mode'.
Node names are best guesses and MUST be verified against the installed grammar.")

(setq sql-ts-mode--indent-rules
  `((sql ; Language Symbol
     ;; Top-level statements start at column 0
     ((parent-is "program") column-0 0)
     ;; Align closing parentheses with the line of the opening one
     ((node-is ")") parent-bol 0)

     ;; Potentially align closing bracket if used (less common in standard SQL)
     ;; ((node-is "]") parent-bol 0)
     
     ;; Comments: Inherit indentation from next non-comment line
     ;; This handles '--' comments well.
     ;; ((node-is "comment") first-sibling 0)

     ;; Between Expression
     ((query ((between_expression (_) @field))) parent-bol 0)
     ;; Parenthesized Expression
     ((query ((parenthesized_expression "(" [(_) _] @body ))) parent-bol sql-ts-mode-indent-offset)
     ;; Binary Expression
     ((query ((binary_expression [(_) _] @field))) first-sibling 0)
     ;; Unary Expression
     ((query ((unary_expression operator: [ (_) _] @operator operand: [(_) _] @operand))) first-sibling 0)
     ;; Select Expression
     ((query ((select_expression (_) @after))) first-sibling 0)
     
     ;; CASE expression
     ((and (parent-is "case") (node-is "keyword_end"))  parent-bol 0)
     ((and (parent-is "case") (node-is "keyword_then"))  parent-bol sql-ts-mode-indent-offset)
     ;; after when
     ((query ((case (keyword_case) (keyword_when) (_) @after-when (keyword_then) ))) (sql-ts-mode--prev-sibling-bol-util "keyword_when")  sql-ts-mode-indent-offset)
     ((parent-is "case") parent-bol sql-ts-mode-indent-offset)
     
     ;; After select
     ((query ((select (_) @after-select))) parent-bol sql-ts-mode-indent-offset)
     ;; After from, (from (keyword_from) (relation xx))
     ((query ((from (keyword_from) [(relation) (join) (cross_join) (lateral_join) (lateral_cross_join)] @parent) )) parent-bol sql-ts-mode-indent-offset)
     ((query ((from (keyword_from) [(where) (group_by) (window_clause) (order_by) (limit)] @parent) )) parent-bol 0)
     ;; After where
     ((parent-is "where") parent-bol sql-ts-mode-indent-offset)
     ;; After group by
     ((parent-is "group_by") parent-bol sql-ts-mode-indent-offset)
     ;; After order by
     ((parent-is "order_by") parent-bol sql-ts-mode-indent-offset)
     ;; After limit
     ((parent-is "limit") parent-bol sql-ts-mode-indent-offset)
     
     ;; After subquery
     ((parent-is "subquery") parent-bol sql-ts-mode-indent-offset)
     ;; After join
     ((parent-is "join") parent-bol 0)

     ;; TODO support dml sql
     
     ;; Try to indent 'sql-ts-mode-indent-offset when else fails
     ((catch-all) prev-line 0)
     ))
  )


;; --- Font Lock ---

;; Mapping from tree-sitter node types to font-lock faces.
;; This needs refinement based on the specific `tree-sitter-sql` grammar
;; and desired highlighting style.
;; --- Font Lock ---

(defvar sql-ts-mode--keywords
  '((keyword_select) (keyword_from) (keyword_where) (keyword_index) (keyword_join)
    (keyword_primary) (keyword_delete) (keyword_create) (keyword_insert) (keyword_merge)
    (keyword_distinct) (keyword_replace) (keyword_update) (keyword_into) (keyword_overwrite)
    (keyword_matched) (keyword_values) (keyword_value) (keyword_attribute) (keyword_set)
    (keyword_left) (keyword_right) (keyword_outer) (keyword_inner) (keyword_full)
    (keyword_order) (keyword_partition) (keyword_group) (keyword_with) (keyword_without)
    (keyword_as) (keyword_having) (keyword_limit) (keyword_offset) (keyword_table)
    (keyword_tables) (keyword_key) (keyword_references) (keyword_foreign) (keyword_constraint)
    (keyword_force) (keyword_use) (keyword_for) (keyword_if) (keyword_exists) (keyword_column)
    (keyword_columns) (keyword_cross) (keyword_lateral) (keyword_natural) (keyword_alter)
    (keyword_drop) (keyword_add) (keyword_view) (keyword_end) (keyword_is) (keyword_using)
    (keyword_between) (keyword_window) (keyword_no) (keyword_data) (keyword_type)
    (keyword_rename) (keyword_to) (keyword_schema) (keyword_owner) (keyword_authorization)
    (keyword_all) (keyword_any) (keyword_some) (keyword_returning) (keyword_begin)
    (keyword_commit) (keyword_rollback) (keyword_transaction) (keyword_only) (keyword_like)
    (keyword_similar) (keyword_over) (keyword_change) (keyword_modify) (keyword_after)
    (keyword_before) (keyword_range) (keyword_rows) (keyword_groups) (keyword_exclude)
    (keyword_current) (keyword_ties) (keyword_others) (keyword_zerofill) (keyword_format)
    (keyword_fields) (keyword_row) (keyword_sort) (keyword_compute) (keyword_comment)
    (keyword_location) (keyword_cached) (keyword_uncached) (keyword_lines) (keyword_stored)
    (keyword_virtual) (keyword_partitioned) (keyword_analyze) (keyword_explain) (keyword_verbose)
    (keyword_truncate) (keyword_rewrite) (keyword_optimize) (keyword_vacuum) (keyword_cache)
    (keyword_language) (keyword_called) (keyword_conflict) (keyword_declare) (keyword_filter)
    (keyword_function) (keyword_input) (keyword_name) (keyword_oid) (keyword_oids)
    (keyword_precision) (keyword_regclass) (keyword_regnamespace) (keyword_regproc) (keyword_regtype)
    (keyword_restricted) (keyword_return) (keyword_returns) (keyword_separator) (keyword_setof)
    (keyword_stable) (keyword_support) (keyword_tblproperties) (keyword_trigger) (keyword_unsafe)
    (keyword_admin) (keyword_connection) (keyword_cycle) (keyword_database) (keyword_encrypted)
    (keyword_increment) (keyword_logged) (keyword_none) (keyword_owned) (keyword_password)
    (keyword_reset) (keyword_role) (keyword_sequence) (keyword_start) (keyword_restart)
    (keyword_tablespace) (keyword_until) (keyword_user) (keyword_valid) (keyword_action)
    (keyword_definer) (keyword_invoker) (keyword_security) (keyword_extension) (keyword_version)
    (keyword_out) (keyword_inout) (keyword_variadic) (keyword_session) (keyword_isolation)
    (keyword_level) (keyword_serializable) (keyword_repeatable) (keyword_read) (keyword_write)
    (keyword_committed) (keyword_uncommitted) (keyword_deferrable) (keyword_names) (keyword_zone)
    (keyword_immediate) (keyword_deferred) (keyword_constraints) (keyword_snapshot) (keyword_characteristics)
    (keyword_off) (keyword_follows) (keyword_precedes) (keyword_each) (keyword_instead)
    (keyword_of) (keyword_initially) (keyword_old) (keyword_new) (keyword_referencing)
    (keyword_statement) (keyword_execute) (keyword_procedure)
    
    ;; conditonal
    (keyword_case) (keyword_when) (keyword_then) (keyword_else)
    ))

(defvar sql-ts-mode--attribute
  '((keyword_asc) (keyword_desc) (keyword_terminated) (keyword_escaped) (keyword_unsigned)
    (keyword_nulls) (keyword_last) (keyword_delimited) (keyword_replication) (keyword_auto_increment)
    (keyword_default) (keyword_collate) (keyword_concurrently) (keyword_engine) (keyword_always)
    (keyword_generated) (keyword_preceding) (keyword_following) (keyword_first) (keyword_current_timestamp)
    (keyword_immutable) (keyword_atomic) (keyword_parallel) (keyword_leakproof) (keyword_safe)
    (keyword_cost) (keyword_strict))
  "SQL attribute for tree-sitter font-locking.")

(defvar sql-ts-mode--variable
  '((term) (field) (relation) (column_definition))
  "SQL variable for tree-sitter font-locking.")

;; Define the font-lock settings using treesit-font-lock-rules.
(defvar sql-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   ;; Comments
   :language 'sql
   :feature 'comment
   '([(comment) (marginalia)] @font-lock-comment-face)

   ;; Keyword
   :language 'sql
   :feature 'keyword
   `([,@sql-ts-mode--keywords] @font-lock-keyword-face
     [(keyword_case) (keyword_when) (keyword_then) (keyword_else)] @font-lock-keyword-face
     )

   ;; Attribute
   :language 'sql
   :feature 'attribute
   `([,@sql-ts-mode--attribute]@font-lock-keyword-face)

   ;; Bracket
   :language 'sql
   :feature 'bracket
   '((["(" ")"]) @font-lock-bracket-face)

   ;; Variable
   :language 'sql
   :feature 'variable
   `([,@sql-ts-mode--variable] @font-lock-variable-name-face
     (term alias: (identifier) @font-lock-variable-name-face)
     (parameter) @font-lock-variable-use-face
     )

   ;; Function call
   :language 'sql
   :feature 'function
   '(
     (invocation (object_reference name: (identifier) @font-lock-function-call-face))
     ;; cast as function
     (term
      value: (cast name: (keyword_cast) @font-lock-function-call-face
	      parameter: (literal) :?)) @font-lock-function-call-face
     [(keyword_gist) (keyword_btree) (keyword_hash) (keyword_spgist)
      (keyword_gin) (keyword_brin) (keyword_array)] @font-lock-function-call-face
     )
   
   ;; Constants
   :language 'sql
   :feature 'string
   '(((literal) @font-lock-string-face
      (:match "\".*?\"" @font-lock-string-face)))

    ;; Constants
   :language 'sql
   :feature 'constant
   '([(keyword_true) (keyword_false) ] @font-lock-constant-face)

   ;; Number
   :language 'sql
   :feature 'number
   '(((literal) @font-lock-number-face
      (:match "^[-]?\\d*" @font-lock-number-face)))

   ;; Operator
   :language 'sql
   :feature 'operator
   '(
     (["+" "-" "*" "/" "%" "^" ":=" "=" "<" "<=" "!=" ">=" ">" "<>"
       (op_other)
       (op_unary_other)]) @font-lock-operator-face
     [
      (keyword_in) (keyword_and) (keyword_or) (keyword_not) (keyword_by)
      (keyword_on) (keyword_do) (keyword_union) (keyword_except) (keyword_intersect)] @font-lock-operator-face
     )

   ;; Delimiter
   :language 'sql
   :feature 'delimiter
   '([";" "," "."] @font-lock-delimiter-face)

   ;; Type
   :language 'sql
   :feature 'type
   '([
      (keyword_int) (keyword_null) (keyword_boolean) (keyword_binary) (keyword_varbinary)
      (keyword_image) (keyword_bit) (keyword_inet) (keyword_character) (keyword_smallserial)
      (keyword_serial) (keyword_bigserial) (keyword_smallint) (keyword_mediumint) (keyword_bigint)
      (keyword_tinyint) (keyword_decimal) (keyword_float) (keyword_double) (keyword_numeric)
      (keyword_real) (double) (keyword_money) (keyword_smallmoney) (keyword_char)
      (keyword_nchar) (keyword_varchar) (keyword_nvarchar) (keyword_varying) (keyword_text)
      (keyword_string) (keyword_uuid) (keyword_json) (keyword_jsonb) (keyword_xml)
      (keyword_bytea) (keyword_enum) (keyword_date) (keyword_datetime) (keyword_time)
      (keyword_datetime2) (keyword_datetimeoffset) (keyword_smalldatetime) (keyword_timestamp) (keyword_timestamptz)
      (keyword_geometry) (keyword_geography) (keyword_box2d) (keyword_box3d) (keyword_interval)] @font-lock-type-face
      (object_reference
       name: (identifier) @font-lock-type-face)
      )

   :language 'sql
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)
   
   )
  "Tree-sitter font-lock rules for `sql-ts-mode'.")


;; --- Major Mode Definition ---

;;;###autoload
(define-derived-mode sql-ts-mode prog-mode "SQL"
  "Major mode for editing SQL files using tree-sitter.
Requires the `sql' tree-sitter grammar to be installed."
  :syntax-table sql-ts-mode-syntax-table

  (when (treesit-ready-p 'sql)

    ;; Set up comment variables
    (setq-local comment-start "-- ")
    (setq-local comment-end "")
    (setq-local comment-style 'indent) ; Preference for comment-dwim

    ;; Imenu
    
    ;; indentation.
    (setq-local indent-line-function #'indent-relative) ; Simple starting point
    (setq-local evil-shift-width sql-ts-mode-indent-offset) ; If using evil-mode
    
    (setq-local indent-tabs-mode nil
		treesit-simple-indent-rules sql-ts-mode--indent-rules)
    
    ;; Font-lock.
    (setq-local treesit-font-lock-settings sql-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string)
                  (assignment attribute builtin constant escape-sequence
			      number type)
                  (bracket delimiter error function operator property variable)))
    (setq-local treesit-font-lock-level 3)

    ;; Electric.
    (setq-local electric-indent-chars
                (append "[]()" electric-indent-chars))

    ;; Tree-sitter setup
    ;; This MUST be called for the mode to work with tree-sitter.
    ;; It sets up font-lock, indentation, imenu, navigation based on tree-sitter.
    (treesit-major-mode-setup)

    ;; Set indentation variable for Emacs's default indentation commands
    ;; Note: For precise indentation, `treesit-indent-rules` should be configured.
    ;; This basic setting might not work well for complex SQL.
    (setq-local tab-width sql-ts-mode-indent-offset)
    (setq-local indent-tabs-mode nil) ; Use spaces for indentation

    ;; Add more mode-specific setup here if needed (keybindings, etc.)
    ))

;;;###autoload
(derived-mode-add-parents 'sql-ts-mode '(sql-mode))
;; (add-to-list 'major-mode-remap-alist '(sql-mode . sql-ts-mode))

;; Add association to file extensions
;;;###autoload
(when (treesit-ready-p 'sql)
  (add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ddl\\'" . sql-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.dml\\'" . sql-ts-mode)))

(provide 'sql-ts-mode)

;;; sql-ts-mode.el ends here
