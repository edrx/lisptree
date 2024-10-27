;; This file:
;;   http://anggtwu.net/luatree/lisptree.lisp.html
;;   http://anggtwu.net/luatree/lisptree.lisp
;;          (find-angg "luatree/lisptree.lisp")
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;;
;; The functions in thie library do these conversions,
;;
;;                       (("f" "__.")                f__.   
;;			  ("|" "  " "|")   	     |  |   
;;   (f a (g b c))  ==>   ("a" "  " "g" "__.")  ==>  a  g__.
;;			  ("|" "  " "|")	        |  |
;;			  ("b" "  " "c"))	        b  c
;;
;;      Lispy tree  ==>       lines             ==> 2D tree
;;
;; from "Lispy trees" to a "lines object" and then to a "2D tree",
;; that is a string with newlines.
;;
;; They were inspired by this Emacs package:
;;   http://anggtwu.net/show-conses.html
;; but here we don't support text properties.
;;
;; This is the inner part of LispTree.
;; The other parts are here:
;;   middle: (find-lisptree "lisptree-middle.lisp")
;;    outer: (find-lisptree "lisptree.mac")
;;
;; (find-showconses "show-conses.el" "lr")
;; (find-es "lisp" "mapconcat")

;; «.package»		(to "package")
;; «.mapconcat»		(to "mapconcat")
;; «.demo-lines»	(to "demo-lines")
;; «.toplain»		(to "toplain")
;; «.width»		(to "width")
;; «.pad»		(to "pad")
;; «.lr»		(to "lr")
;; «.tree»		(to "tree")



;; «package»  (to ".package")
;;
(defpackage :lisptree (:use :common-lisp))
(in-package :lisptree)


;; «mapconcat»  (to ".mapconcat")
;;
(defvar newline (format nil "~%"))

(defun concat (&rest strs)
  (apply 'concatenate 'string strs))

(defun mapconcat (f as sep)
  (if (eq () as) ""
      (let* ((bs  (map 'list f as))
	     (sbs (loop for b in (cdr bs)
			collect sep
			collect b))
	     (bsbs (cons (car bs) sbs)))
	(apply 'concat bsbs))))

#|
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
(defun f (o) (format nil "[~a]" o))
(mapconcat #'f '(2 3 4) "_")
(mapconcat #'f '(2 3 4) newline)

|#


;; «demo-lines»  (to ".demo-lines")
;; (find-showconses "show-conses.el" "demo-lines")
;;
(defvar demo-lines-1)
(setq demo-lines-1
      '(("a" "__" "b")
	("|")
	("c")))


;; «toplain»  (to ".toplain")
;; (find-showconses "show-conses.el" "toplain")
;;
(defun toplain-line (line)
  (apply 'concat line))

(defun toplain-lines (lines)
  (mapconcat 'toplain-line lines newline))

#|
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
(toplain-line (car demo-lines-1))
(toplain-lines     demo-lines-1)

|#


;; «width»  (to ".width")
;; (find-showconses "show-conses.el" "width")

(defun width-line (line)
  (length (toplain-line line)))

(defun widths-of-lines (lines)
  (mapcar 'width-line lines))

(defun width-lines (lines)
  (apply 'max (widths-of-lines lines)))

#|
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
(width-lines demo-lines-1)

|#


;; «pad»  (to ".pad")
;; (find-showconses "show-conses.el" "pad")

(defun pad-line (wtotal line &optional char)
  "Pad LINE to the width WTOTAL."
  (let* ((wleft  (width-line line))
	 (wright (- wtotal wleft))
	 (spaces (make-string wright :initial-element (or char #\ ))))
    (if (< wleft wtotal)
	(append line (list spaces))
      line)))

(defun pad-lines (lines)
  (let ((maxwidth (width-lines lines)))
    (loop for line in lines
          collect (pad-line maxwidth line))))

#|
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
(pad-lines demo-lines-1)

|#



;; «lr»  (to ".lr")
;;
(defun add-pin (newtopline lines)
  `(,newtopline ("|") ,@lines))

(defun add-hline (lines &optional wtotal)
  (setq wtotal (or wtotal (+ 2 (width-lines lines))))
  (let* ((topline    (car lines))
         (otherlines (cdr lines))
         (newtopline (pad-line wtotal topline #\_)))
    `(,newtopline ,@otherlines)))

(defun pad-bottom (lines newheight)
  (let ((currentheight (length lines)))
    (if (>= currentheight newheight)
	lines
      (let ((newlines (make-list (- newheight currentheight) :initial-element ())))
	`(,@lines ,@newlines)))))

(defun lr (leftlines rightlines)
  (let* ((leftheight  (length leftlines))
         (rightheight (length rightlines))
	 (maxheight   (max leftheight rightheight))
	 (leftlines2  (pad-bottom leftlines  maxheight))
	 (rightlines2 (pad-bottom rightlines maxheight))
	 (leftlines3  (pad-lines  leftlines2)))
    (loop for l in leftlines3
	  for r in rightlines2
	  collect `(,@l ,@r))))

(defun l_r (leftlines rightlines)
  (let ((leftlines_ (add-hline leftlines)))
    (lr leftlines_ rightlines)))

#|
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
(defvar demo-lines-2)
(setq   demo-lines-2 (l_r '(("d")) demo-lines-1))
(toplain-lines demo-lines-2)
(princ (toplain-lines demo-lines-2))

(if t 1 2 3)

|#



;; «tree»  (to ".tree")
;;
(defun lispytree-atom (o)
  "Convert a Lispy tree atom `o' to a lines object."
  (if (stringp o)
      `((,o))
      `((,(format nil "~s" o)))))

(defun lispytree-opargs (op args)
  "Convert a Lispy tree of the form (op arg1 arg2 ...) to a lines object."
  (let* ((pin (car (lispytree-atom op)))
	 (nargs (length args)))
    (if (eq nargs 0)
	(let* ((down (lispytree-atom " ")))
	  (add-pin pin down))
	(let* ((down  (lispytree (car args)))
	       (ltree (add-pin pin down)))
	  (if (eq nargs 1)
	      ltree
	      (let ((rtree (lispytree-opargs "." (cdr args))))
		(l_r ltree rtree)))))))

(defun lispytree (o)
  "Convert a Lispy tree `o' to a lines object."
  (if (listp o)
      (lispytree-opargs (car o) (cdr o))
      (lispytree-atom o)))


#|
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(add-pin "*" '())

(lispytree-atom   "*")
(lispytree-atom   " ")
(lispytree-opargs "*" '(2 3))
(lispytree-opargs "*" '(2))
(lispytree-opargs "*" '())

(length '(a b c d))
'()
()

(lispytree 2)
(lispytree '(2 3))
(lispytree '(2 3 4))
(lispytree '(2 3 (4 5 6)))
(toplain-lines (lispytree '(2 3 (4 5 6))))
(toplain-lines (lispytree '(2 3 (4 5 "6"))))

|#



#|
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(apropos "simplifymaxima")
(apropos "mlist")

(defun foo::bar ())

|#


#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("~/luatree/lisptree.lisp");
lisptree(2);
?lisptree(2);
to_lisp();
  (apropos "lisptree")
  (apropos "simplifymaxima")
  (packagep "lisptree")
  (find-package "lisptree")
  (find-package :lisptree)
  (describe 'lisptree::lr)
  (apropos "simplifymaxima")
  #$[x, y, z]$
  (simplifymaximatree 2)

|#






;; Local Variables:
;; coding:  utf-8-unix
;; End:
