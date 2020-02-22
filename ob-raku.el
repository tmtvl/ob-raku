;;; ob-raku.el --- org-babel functions for Raku evaluation

;; Copyright (C) Tim Van den Langenbergh

;; Author: Tim Van den Langenbergh <tmt_vdl@gmx.com>
;; Keywords: literate programming, reproducible research
;; Homepage: https://github.com/tmtvl/ob-raku
;; Version: 0.04
;; News: 0.04 --- Added square brackets to list splitting, so as to split embedded arrays as well as lists.
;;       0.03 --- Removed the double execution, simplified the formatting of the Raku output, fixed hline support.
;;       0.02 --- Added support for tables, removed unneeded require statements, error when trying to use a session.
;;       0.01 --- Initial release. Accept inputs, support for output and value results.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Bindings for org-babel support for Raku (née Perl6).

;;; Requirements:

;; Requires a working Raku interpreter to be installed.
;; (Optionally) requires Perl6 mode to be installed.

;;; Code:

(require 'ob)
(require 'ob-eval)
(require 'ob-ref)

(add-to-list 'org-babel-tangle-lang-exts '("raku" . "raku"))

(defvar org-babel-default-header-args:raku '())

(defvar org-babel-raku-command "raku"
  "Command to run Raku.")

(defun org-babel-expand-body:raku (body params &optional processed-params)
  "Expand BODY according to the header arguments specified in PARAMS.
Use the PROCESSED-PARAMS if defined."
  (let ((vars
	 (delq nil
	       (mapcar
		(lambda (pair)
		  (when (eq (car pair) :var) (cdr pair)))
		(or processed-params (org-babel-process-params params))))))
    (concat
     (mapconcat
      (lambda (pair)
	(format
	 "my %s%s = %s;"
	 (if (listp (cdr pair))
	     "@"
	   "$")
	 (car pair)
	 (org-babel-raku-var-to-raku (cdr pair))))
      vars
      "\n")
     "\n" body "\n")))

(defun org-babel-execute:raku (body params)
  "Execute the BODY of Raku code processing it according to PARAMS."
  (let* ((processed-params (org-babel-process-params params))
	 (session (cdr (assoc :session params)))
	 (result-type (cdr (assoc :result-type params)))
	 (result-params (cdr (assoc :result-params params)))
	 (full-body (org-babel-expand-body:raku body params processed-params)))
    (org-babel-reassemble-table
     (org-babel-raku-evaluate full-body session result-type)
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defun org-babel-prep-session:raku (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Sessions are not supported for Raku"))

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

(defun org-babel-raku-sanitize-table (table)
  "Recursively sanitize the values in the given TABLE."
  (if (listp table)
      (let ((sanitized-table (mapcar 'org-babel-raku-sanitize-table table)))
	(if (and (stringp (car sanitized-table))
		 (string= (car sanitized-table) "HLINE"))
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
     (mapcar
      (lambda (pairstring)
	(split-string pairstring ", " t))
      (split-string
       (substring results 2 -2)
       "[][()]"
       t))))
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

(defun org-babel-raku-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized SESSION."
  (unless (string= session "none")
    (if session (error "Sessions are not supported for Raku"))))

(defun org-babel-raku-evaluate (body &optional session result-type)
  "Evaluate the BODY with Raku.
If SESSION is not provided, evaluate in an external process.
If RESULT-TYPE is not provided, assume \"value\"."
  (org-babel-raku-table-or-string (if (and session (not (string= session "none")))
      (org-babel-raku-evaluate-session session body result-type)
    (org-babel-raku-evaluate-external body result-type))))

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

(defun org-babel-raku-evaluate-session (session body &optional result-type)
  "Evaluate the BODY with the Raku process running in SESSION.
If RESULT-TYPE is not provided, assume \"value\"."
  (error "Sessions are not supported for Raku"))

(provide 'ob-raku)
;;; ob-raku.el ends here
