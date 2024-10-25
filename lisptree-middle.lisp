;; This file:
;;   http://anggtwu.net/lisptree/lisptree-middle.lisp.html
;;   http://anggtwu.net/lisptree/lisptree-middle.lisp
;;          (find-angg "lisptree/lisptree-middle.lisp")
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;;
;; This file, lisptree-middle.lisp, is the middle part of LispTree -
;; it implements three Lisp functions callable from Maxima that
;; access functions defined in lisptree-middle.lisp.
;;
;; (defun ll () (interactive) (find-lisptreefile "lisptree.lisp"))
;; (defun l1 () (interactive) (find-lisptreefile "lisptree-middle.lisp"))
;; (find-angg "MAXIMA/mycolorlerp1.lisp")

(in-package :maxima)

(defun simplifytree (o)
  "This function converts a Maxima tree O to a lispy tree.
For example, (simplifytree #$[f,2,[g,3,4]]$)
returns:                    ($F 2 ($G 3 4))"
  (if (and (listp o) (eq (caar o) 'mlist))
      (map 'list #'simplifytree (cdr o))
      o))

#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("lisptree.lisp");
load("lisptree-middle.lisp");
to_lisp();
                #$[f,2,[g,3,4]]$
  (simplifytree #$[f,2,[g,3,4]]$)
  (to-maxima)

|#

(defun $lisptree1__ (o)
"This function converts a Maxima tree O to a lispy tree converted to a string.
For example, lisptree1__([f,2,[g,3,4]])
returns               \"($F 2 ($G 3 4))\".
This is mainly for debugging."
  (format nil "~S" (simplifytree o)))

(defun $lisptree1_ (o)
"This function converts a Maxima tree O to a 2D tree.
For example, lisptree1__([f,2,[g,3,4]])
return (roughly) this:
\"$F__.
 |   |
 2   $G__.
     |   |
     3   4\""
  (lisptree::toplain-lines (lisptree::maxima (simplifytree o))))

(defun $lisptree1 (o)
"This is like `$lisptree1_', but adds an initial newline."
  (format nil "~%~a" ($lisptree1_ o)))


#|
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("lisptree.lisp");
load("lisptree-middle.lisp");
lisptree1__([f,2,[g,3,4]]);
lisptree1_ ([f,2,[g,3,4]]);
lisptree1  ([f,2,[g,3,4]]);

lisptree1__([f, 4, "5", [g, foo, ?foo]]);
lisptree1_ ([f, 4, "5", [g, foo, ?foo]]);
lisptree1  ([f, 4, "5", [g, foo, ?foo]]);

|#



;; Local Variables:
;; coding:  utf-8-unix
;; End:
