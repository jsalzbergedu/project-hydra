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
;;  :compile compile-fun
;;  :run run-fun
;;  :and ("s" style-check-fun))
;;
;; which becomes
;;
;;   (defhydra hydra-java (:color blue :hint nil)
;;     ("c" compile-fun)
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
;;   :compile sillyfun
;;   :run sillyfun
;;   :stylecheck sillyfun
;;   :search sillyfun
;;   :git sillyfun
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
;; _c_ sillyfun
;; _r_ sillyfun
;; _s_ sillyfun
;; _g_ sillyfun
;; _m_ sillyfun
;; _v_ sillyfun
;; "
;;   ("t" sillyfun)
;;   ("c" sillyfun)
;;   ("r" sillyfun)
;;   ("s" sillyfun)
;;   ("m" sillyfun)
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
    (:compile "c")
    (:run "r")
    (:stylecheck "s")
    (:search "g")
    (:git "m"))
  "The rules that bind a key (in the symbol sense) (e.g. :test) to a key (in the keyboard sense).")

(defun project-hydra--and-rule (sexp)
  "The rule for :add (SEXP is simply not touched)."
  (when (eq (car sexp) ':and)
    (cadr sexp)))

(defun project-hydra--parse-one-sexp (sexp)
  "Parse one SEXP from the body."
  (let* ((normal-rules (cl-map 'list 'project-hydra--general-rule-list project-hydra-rules))
	 (rules (cons 'project-hydra--and-rule normal-rules)))
    (car (cl-remove-if-not 'identity (cl-map 'list (lambda (a) (funcall a sexp)) rules)))))


(defun project-hydra--pair-off (list)
  "Pair off every two elemetns in LIST.
Returns nill if the list is not even."
  (when (cl-evenp (length list))
    (let ((list list) pairs)
      (while list
	(push (list (pop list) (pop list)) pairs))
      (nreverse pairs))))

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

(defun project-hydra--rightpad-to (s len)
  "Right pad S until it is LEN spaces."
  (let ((extra-len (- len (length s))))
    (if (<= extra-len 0)
	s
      (project-hydra--rightpad s extra-len))))

(defun project-hydra--docstring-rule (sexp)
  "Generate a docstring from SEXP of the parsed results of project-hydra--parse-body."
  (format "_%s_ %s " (car sexp) (cadr sexp)))

(defvar project-hydra-docstring-header (concat "\n"
					       "^Project Operations and Tasks^\n"
					       "----------------------------\n")
  "The docstring header for the project hydra")

(defun project-hydra--cleave (list)
  "Cleave even length LIST into two lists."
  (let ((halflen (/ (length list) 2))
	(list list)
	(i 0)
	fst)
    (while (< i halflen)
      (cl-incf i)
      (push (pop list) fst))
    (list (nreverse fst) list)))

(defun project-hydra--longest (list)
  "Return the length of the longest string in LIST." 
  (car (sort (cl-map 'list 'length list) '>)))

(defun project-hydra--gen-docstring-even (body)
  "Generate a docstring from the parsed results of BODY, assuming that body has an even number of members."
  (let* ((lists (project-hydra--cleave (cl-map 'list 'project-hydra--docstring-rule body)))
	 (fst (car lists))
	 (snd (cadr lists))
	 (fst-longest-len (project-hydra--longest fst))
	 (rightpad (lambda (s) (project-hydra--rightpad-to s fst-longest-len)))
	 (fst-equal-lens (cl-map 'list rightpad fst))
	 (combined (cl-map 'list (lambda (a) (concat a "\n")) (cl-map 'list 'concat fst-equal-lens snd))))
    (mapc 'message fst-equal-lens)
    (list (cl-reduce 'concat combined :initial-value project-hydra-docstring-header))))

(defun project-hydra--gen-docstring (body)
  "Generate a docstring from the parsed results of BODY."
  (if (cl-evenp (length body))
      (project-hydra--gen-docstring-even body)
    (let ((last (last body))
	  (butlast (butlast body)))
      (list (apply 'concat (append (project-hydra--gen-docstring-even butlast)
				   (list (apply 'project-hydra--docstring-rule last))))))))

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
