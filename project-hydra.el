;; -*- lexical-binding: t -*-
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

(require 'hydra)
(require 'dash)

(defun project-hydra--general-rule (sym key)
  "The general rule: (:sym . key) which will be applied to sexp"
  (lambda (sexp)
    (when (eq (car sexp) sym)
      (list key (cadr sexp)))))

(defun project-hydra--general-rule-list (list)
  "The unapplied version of project-hydra--gerneral-rule"
  (apply 'project-hydra--general-rule list))

(defvar project-hydra-rules
  '((:test "t")
    (:build "b")
    (:run "r")
    (:stylecheck "s"))
  "The rules that bind a key (in the symbol sense) (e.g. :test) to a key (in the keyboard sense).")


(defun project-hydra--and-rule (sexp)
  "The rule for :add (it simply is not touched)"
  (when (eq (car sexp) ':and)
    (cadr sexp)))

(message "%s" (append '(1 2) '(1)))

(defun project-hydra--parse-one-sexp (sexp)
  "Parse one sexp from the body"
  (let* ((normal-rules (map 'list 'project-hydra--general-rule-list project-hydra-rules))
	 (rules (cons 'project-hydra--and-rule normal-rules)))
    (car (cl-remove-if-not 'identity (map 'list (lambda (a) (funcall a sexp)) rules)))))

(defun pair-off (list)
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
  "Parse the body of the macro"
  (map 'list 'project-hydra--parse-one-sexp (pair-off body)))

(defun project-hydra--begin-body (name)
  `(defhydra ,name (:color blue :hint nil)))

(defmacro project-hydra (name &rest body)
  "A template for generating hydras for projects in different languages.
To use, either use one of the keys defined in project-hydra-rules like this:
(project-hydra :test test-fun
:build build-fun
:run run-fun
:stylecheck stylecheck-fun
:and (key . other-binding)
:and (key . and-so-on))
"
  (declare (indent defun))
  (append (project-hydra--begin-body name) (project-hydra--parse-body body)))

(defun sillyfun ()
  (interactive)
  (message "hi!"))

(project-hydra doot-hydra
  :test sillyfun
  :build sillyfun
  :run sillyfun
  :stylecheck sillyfun
  :and ("v" sillyfun))
