;;; project-hydra --- hyrda skeleton for disparate modes -*- lexical-binding: t -*-
;;; Commentary:
;; A hydra skeleton for projects.

;; Will support mainly operations on whole projects, such as:
;; Test
;; Build
;; Style check
;; Run
;; The syntax will hopefully look like this
;;
;; (project-hydra hydra-java
;;  :test test-fun
;;  :build build-fun
;;  :run run-fun
;;  :and ("s" style-check-fun))
;;
;; which becomes
;;
;;   (defhydra hydra-java (:color blue :hint nil)
;;     ("b" build-fun)
;;     ("t" test-fun)
;;     ("r" run-fun)
;;     ("s" style-check-fun))
;;
;; And one can ues less than all of the tags, e.g.
;; (project-hydra
;;  :test test-fun)

;; Full example
;; 
;; (defun sillyfun ()
;;   (interactive)
;;   (message "hi!"))

;; (project-hydra doot-hydra
;;   :test sillyfun
;;   :build sillyfun
;;   :run sillyfun
;;   :stylecheck sillyfun
;;   :and ("v" sillyfun))
;;
;;
;; Becomes
;;
;; (defhydra doot-hydra
;;   (:color blue :hint nil)
;;   "
;; ^Project Operations and Tasks^
;; ----------------------------
;; _t_ sillyfun
;; _b_ sillyfun
;; _r_ sillyfun
;; _s_ sillyfun
;; _v_ sillyfun
;; "
;;   ("t" sillyfun)
;;   ("b" sillyfun)
;;   ("r" sillyfun)
;;   ("s" sillyfun)
;;   ("v" sillyfun))

;;; Code:

(require 'hydra)
(require 'cl-lib)

(defun project-hydra--general-rule (sym key)
  "The general rule for (:SYM . KEY), used in code generation for the keybinding."
  (lambda (sexp)
    (when (eq (car sexp) sym)
      (list key (cadr sexp)))))

(defun project-hydra--general-rule-list (list)
  "The unapplied version of project-hydra--gerneral-rule, applied to LIST."
  (apply 'project-hydra--general-rule list))

(defvar project-hydra-rules
  '((:test "t")
    (:build "b")
    (:run "r")
    (:stylecheck "s"))
  "The rules that bind a key (in the symbol sense) (e.g. :test) to a key (in the keyboard sense).")

(defun project-hydra--and-rule (sexp)
  "The rule for :add (SEXP is simply not touched)."
  (when (eq (car sexp) ':and)
    (cadr sexp)))

(defun project-hydra--parse-one-sexp (sexp)
  "Parse one SEXP from the body."
  (let* ((normal-rules (cl-map 'list 'project-hydra--general-rule-list project-hydra-rules))
	 (rules (cons 'project-hydra--and-rule normal-rules)))
    (message "%s" rules)
    (car (cl-remove-if-not 'identity (cl-map 'list (lambda (a) (funcall a sexp)) rules)))))

(defun project-hydra--pair-off (list)
  "Pair off every two elements in LIST."
  (when (= (% (length list) 2) 0)
    (let ((flag t)
	  (acc '())
	  (list list))
      (while flag
	(setq acc (append acc (list (list (car list) (cadr list)))))
	(setq list (cddr list))
	(when (= 0 (length list))
	  (setq flag nil)))
      acc)))

(defun project-hydra--parse-body (body)
  "Parse the BODY of the macro."
  (cl-map 'list 'project-hydra--parse-one-sexp (project-hydra--pair-off body)))

(defun project-hydra--begin-body (name)
  "Begin the body of the hydra, taking argument NAME."
  `(defhydra ,name (:color blue :hint nil)))

(defun project-hydra--rightpad (s len)
  "Right pad S with LEN spaces."
  (concat s (cl-loop repeat len concat " ")))

(defun project-hydra--docstring-rule (sexp)
  "Generate a docstring from SEXP of the parsed results of project-hydra--parse-body."
  (format "_%s_ %s\n" (car sexp) (cadr sexp)))

(defun project-hydra--gen-docstring (body)
  "Generate a docstring from the parsed results of BODY."
  (list (cl-reduce 'concat (cl-map 'list 'project-hydra--docstring-rule body)
		   :initial-value (concat "\n"
					  "^Project Operations and Tasks^\n"
					  "----------------------------\n"))))

(defmacro project-hydra (name &rest body)
  "A template for generating for projects in different languages.
NAME is used for the name, and BODY is parsed into the hydra's body.
To use, either use one of the keys defined in project-hydra-rules like this:
\(project-hydra :test test-fun
:build build-fun
:run run-fun
:stylecheck stylecheck-fun
:and \(key . other-binding\)
:and \(key . and-so-on\)\)"
  (declare (indent defun))
  (let ((parsed (project-hydra--parse-body body)))
    (append (project-hydra--begin-body name) (project-hydra--gen-docstring parsed) parsed)))

(provide 'project-hydra)
;;; project-hydra.el ends here
