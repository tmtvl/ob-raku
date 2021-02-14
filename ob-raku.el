;;; ob-raku.el --- org-babel functions for Raku evaluation

;; Copyright (C) Tim Van den Langenbergh

;; Author: Tim Van den Langenbergh <tmt_vdl@gmx.com>
;; Keywords: literate programming, reproducible research
;; Homepage: https://github.com/tmtvl/ob-raku
;; Version: 0.7
;; Package-Requires: ((emacs "26.1") raku-mode)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; News:

;;; Commentary:

;; Bindings for org-babel support for Raku (née Perl6).

;;; Requirements:

;; Requires a working Raku interpreter to be installed.
;; Requires Raku mode to be installed.

;;; Code:

(require 'avl-tree)
(require 'ob)
(require 'ob-eval)
(require 'ob-ref)
(require 'raku-repl nil t)

(defvar raku-buffer-name) ; Defined in raku-repl.el

(declare-function run-raku
				  "ext:raku-repl"
				  ())

(add-to-list 'org-babel-tangle-lang-exts '("raku" . "raku"))

(defvar org-babel-default-header-args:raku '()
  "Default header arguments for Raku code blocks.")

(defvar org-babel-raku-command "raku"
  "Command to run Raku.")

(defvar raku-sessions
  (avl-tree-create #'string<)
  "Active Raku sessions.")

(defvar org-babel-raku-eoe-indicator ":org_babel_raku_eoe"
  "String to indicate that evaluation has completed.")

(defconst org-babel-raku-wrapper
  "sub _MAIN {
%s
}

sub _FORMATTER ($result) {
return $result.gist if $result.WHAT ~~ Hash;
$result.raku
}

\"%s\".IO.spurt(\"{ _FORMATTER(_MAIN()) }\\n\");"
  "Wrapper for grabbing the final value from Raku code.")

(defun org-babel-variable-assignments:raku (params &optional processed-params)
  "Return a list of Raku statements assigning the block's variables."
  (mapcar
   (lambda (pair)
	 (format "my %s%s = %s;"
			 (if (listp (cdr pair))
				 "@"
			   "$")
			 (car pair)
			 (org-babel-raku-var-to-raku (cdr pair))))
   (org-babel--get-vars
	(or processed-params
		(org-babel-process-params params)))))

(defun org-babel-expand-body:raku (body params &optional processed-params)
  "Expand BODY according to the header arguments specified in PARAMS.
Use the PROCESSED-PARAMS if defined."
  (concat
   (mapconcat
	(lambda (assignment)
	  assignment)
	(org-babel-variable-assignments:raku params processed-params)
	"\n")
   "\n" body "\n"))

(defun org-babel-execute:raku (body params)
  "Execute the BODY of Raku code processing it according to PARAMS."
  (let* ((processed-params (org-babel-process-params params))
		 (session (org-babel-raku-initiate-session
				   (cdr (assoc :session params))))
		 (result-type (cdr (assoc :result-type params)))
		 (result-params (cdr (assoc :result-params params)))
		 (full-body (org-babel-expand-body:generic
					 body
					 params
					 (org-babel-variable-assignments:raku
					  params
					  processed-params))))
    (org-babel-reassemble-table
     (org-babel-raku-evaluate full-body session result-type)
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defun org-babel-session-name-with-earmuffs (session)
  "Add *earmuffs* to SESSION unless it already has them."
  (if (and (eq (elt session 0) ?\*)
		   (eq (elt session
					(- (length session) 1))
			   ?\*))
	  session
	(format "*%s*" session)))

(defun org-babel-session-name-without-earmuffs (session)
  "Remove *earmuffs* from SESSION if it has them."
  (if (and (eq (elt session 0) ?\*)
		   (eq (elt session
					(- (length session) 1))
			   ?\*))
	  (substring session 1
				 (- (length session) 1))
	session))

(defun org-babel-raku-get-session (session)
  "Look up SESSION in `raku-sessions'."
  (avl-tree-member raku-sessions session))

(defun org-babel-raku-cleanse-session (session)
  "Remove SESSION from `raku-sessions' if the session has been closed."
  (when (org-babel-raku-get-session session)
	(let ((orig-raku-buffer-name raku-buffer-name))
	  (setq raku-buffer-name session)
	  (unless (raku-comint-get-process)
		(avl-tree-delete raku-sessions session))
	  (setq raku-buffer-name orig-raku-buffer-name))))

(defun org-babel-raku-register-session (session)
  "Add SESSION to `raku-sessions' unless it already exists."
  (unless (org-babel-raku-get-session session)
	(avl-tree-enter raku-sessions session)))

(defun org-babel-prep-session:raku (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let ((session (org-babel-raku-initiate-session session))
		(var-lines (org-babel-variable-assignments:raku params)))
	(org-babel-comint-in-buffer session
	  (sit-for .5)
	  (goto-char (point-max))
	  (dolist (var var-lines)
		(insert var)
		(comint-send-input nil t)
		(org-babel-comint-wait-for-output session)
		(sit-for .1)
		(goto-char (point-max))))
	session))

(defun org-babel-raku-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized SESSION."
  (unless (string= session "none")
	(save-window-excursion
	  (let ((session-name (if session
							  (org-babel-session-name-with-earmuffs session)
							(org-babel-session-name-with-earmuffs
							 (buffer-name (current-buffer)))))
			(orig-raku-buffer-name raku-buffer-name))
		(org-babel-raku-cleanse-session session-name)
		(unless (org-babel-raku-get-session session-name)
		  (setq raku-buffer-name session)
		  (run-raku)
		  (setq raku-buffer-name orig-raku-buffer-name))
		session-name))))

(defun org-babel-load-session:raku (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
	(let ((session (org-babel-prep-session:raku session params)))
	  (with-current-buffer session
		(goto-char (process-mark
					(get-buffer-process (current-buffer))))
		(insert (org-babel-chomp body)))
	  session)))

(defun org-babel-raku-evaluate-session (session body &optional result-type)
  "Evaluate the BODY with the Raku process running in SESSION.
If RESULT-TYPE is not provided, assume \"value\"."
  (let ((eoe-string (format "say \"%s\";" org-babel-raku-eoe-indicator)))
	(org-babel-comint-with-output
		(session org-babel-raku-eoe-indicator t nil)
	  (insert eoe-string)
	  (comint-send-input nil t))
	(let ((result (mapcar
				   #'org-trim
				   (butlast
					(org-babel-comint-with-output
						(session org-babel-raku-eoe-indicator t body)
					  (insert body)
					  (comint-send-input nil t)
					  (insert eoe-string)
					  (comint-send-input nil t))
					2))))
	  (if (string= result-type "output")
		  (mapconcat #'identity result "\n")
		(car (last result))))))

(defun org-babel-raku-var-to-raku (var)
  "Convert an elisp value VAR to a Raku definition of the same value."
  (if (listp var)
      (concat
       "("
       (mapconcat
		#'org-babel-raku-var-to-raku
		var
		", ")
       ")")
    (if (equal var 'hline)
		"\"HLINE\""
      (format "%S" var))))

(defun org-babel-raku-escape-nested-list-delimiters (list)
  "Escapes any commas or parentheses found in strings contained in the given \
LIST."
  (let ((in-string nil)
		(last-char nil))
    (concat
     (mapcan
      (lambda (c)
		(cond ((and (eq c ?\")
					(not (eq last-char ?\\)))
			   (setq in-string (not in-string))
			   (setq last-char c)
			   (list c))
			  ((and
				in-string
				(member c (list ?\( ?\) ?\[ ?\] ?,)))
			   (setq last-char c)
			   (list ?\\ c))
			  (t (setq last-char c)
				 (list c))))
      list))))

(defun org-babel-raku-unescape-parens-and-commas (string)
  "Unescapes parentheses and commas in STRING."
  (replace-regexp-in-string "\\\\\\([][(),]\\)" "\\1" string))

(defun org-babel-raku-split-list (list)
  "Split LIST on a comma or parentheses, ignoring those in a string."
  (mapcar
   (lambda (pairstring)
     (mapcar
      (lambda (string)
		(org-babel-raku-unescape-parens-and-commas string))
      (split-string pairstring "[^\\], " t)))
   (split-string (replace-regexp-in-string
				  "\\([^\\$]\\)\\([][(),]\\)"
				  "\\1 \\2"
				  (org-babel-raku-escape-nested-list-delimiters
				   (concat
					" "
					(substring list 2 -2)
					" ")))
				 "[^\\][][()]\\(,\\)?"
				 t
				 split-string-default-separators)))

(defun org-babel-raku-sanitize-table (table)
  "Recursively sanitize the values in the given TABLE."
  (if (listp table)
      (let ((sanitized-table (mapcar 'org-babel-raku-sanitize-table table)))
		(if (equal (car sanitized-table) "HLINE")
			'hline
		  sanitized-table))
    (org-babel-script-escape table)))

(defun org-babel-raku-table-or-string (results)
  "If RESULTS look like a table, then convert them into an elisp table.
Otherwise return RESULTS as a string."
  (cond
   ((or (string-prefix-p "$[" results)
		(string-prefix-p "$(" results))
    (org-babel-raku-sanitize-table
     (org-babel-raku-split-list results)))
   ((string-prefix-p "{" results)
    (org-babel-raku-sanitize-table
     (mapcar
      (lambda (pairstring)
		(split-string pairstring " => " t))
      (split-string
       (substring results 1 -2)
       ", "
       t))))
   (t (org-babel-script-escape results))))

(defun org-babel-raku-evaluate (body &optional session result-type)
  "Evaluate the BODY with Raku.
If SESSION is not provided, evaluate in an external process.
If RESULT-TYPE is not provided, assume \"value\"."
  (let ((result (if (and session (not (string= session "none")))
					(org-babel-raku-evaluate-session session body result-type)
				  (org-babel-raku-evaluate-external body result-type))))
	(if (stringp result)
		(org-babel-raku-table-or-string result)
	  (mapcar #'org-babel-raku-table-or-string result))))

(defun org-babel-raku-evaluate-external (body &optional result-type)
  "Evaluate the BODY with an external Raku process.
If RESULT-TYPE is not provided, assume \"value\"."
  (if (and result-type (string= result-type "output"))
      (org-babel-eval org-babel-raku-command body)
    (let ((temp-file (org-babel-temp-file "raku-" ".raku")))
      (org-babel-eval
       org-babel-raku-command
       (format
		org-babel-raku-wrapper
		body
		(org-babel-process-file-name temp-file 'noquote)))
      (org-babel-eval-read-file temp-file))))

(provide 'ob-raku)
;;; ob-raku.el ends here
