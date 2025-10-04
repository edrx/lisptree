;; This file:
;;   http://anggtwu.net/lisptree/lisptree-middle.lisp.html
;;   http://anggtwu.net/lisptree/lisptree-middle.lisp
;;               (find-lisptree "lisptree-middle.lisp")
;;      See:  http://anggtwu.net/lisptree.html
;;       https://github.com/edrx/lisptree
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;; License: Public Domain.
;; Version: 2024oct27.
;;
;; This is the middle-level part of Lisptree. The main top-level
;; function in this file is `tolisptree2d', that works like this:
;;
;;   tolisptree2d ([f,2,[g,3,4]]);
;;
;;        $F__.
;;        |   |
;;   ==>  2   $G__.
;;            |   |
;;            3   4
;;
;;    tolisptree2d ([f, 4, "5", [g, foo, ?foo, Foo]]);
;;
;;        $F__.__.
;;        |   |  |
;;   ==>  4   5  $G____.____.
;;               |     |    |
;;               $FOO  FOO  |$Foo|
;;
;; `tolisptree2d' receives a Maxima object that "represents a tree"
;; and returns a string with newlines that is that Maxima object
;; "drawn as a tree".
;;
;; Note that `tolisptree2d'
;;   receives this:         [f,2,[g,3,4]]
;;    and NOT this:         f(2,g(3,4))
;; That's because this conversion
;;                          f(2,g(3,4))
;;                       -> [f,2,[g,3,4]]
;; is very tricky...
;; See: (find-lisptree "lisptree.mac")

;; This file, lisptree-middle.lisp, is the middle part of LispTree -
;; it implements some Lisp functions callable from Maxima that access
;; functions defined in lisptree.lisp.
;;
;; Links:
;; The middle part of lisptree is this file.
;; The outer  part of lisptree is: (find-lisptree "lisptree.mac")
;; The inner  part of lisptree is: (find-lisptree "lisptree.lisp")
;;
;; A "Maxima object" is something like f(2,g(3,4)).
;; A "Maxima tree"   is something like [f,2,[g,3,4]].
;; A "Lispy tree "   is something like ($F 2 ($G 3 4)).
;; A "2D tree"       is something like this,
;;
;;     f__.
;;     |  |
;;     2  g__.
;;        |  |
;;        3  4
;;
;; as a string with newlines.
;; Here is a map of the conversions:
;;
;;      Maxima object --> Maxima tree --> Lispy tree --> 2D tree
;;
;; Here - in lisptree-middle.lisp - we do these ones:
;;
;;    tolispytree:        Maxima tree --> Lispy tree
;;   $tolispytreestr:     Maxima tree --> Lispy tree --> string
;;   $tolispytree2d_:     Maxima tree --> Lispy tree --> 2D tree
;;   $tolispytree2d:      Maxima tree --> Lispy tree --> 2D tree (with newline)

;; «.package»		(to "package")
;; «.tolispytree»	(to "tolispytree")
;; «.tolispytree-tests»	(to "tolispytree-tests")
;; «.maxima»		(to "maxima")
;; «.maxima-tests»	(to "maxima-tests")


;; «package»  (to ".package")
;; The functions in this file are put in the package `:maxima'.
;; The functions in lisptree.lisp are put in the package `:lisptree'.
;; See: (find-lisptree "lisptree.lisp" "package")
(in-package :maxima)


;; «tolispytree»  (to ".tolispytree")
(defun tolispytree (o)
  "This function converts a Maxima tree `o' to a Lispy tree.
For example, (tolispytree #$[f, 2,[g, 3, 4]]$)
returns:                    ($F 2 ($G 3 4))"
  (if (and (listp o) (eq (caar o) 'mlist))
      (map 'list #'tolispytree (cdr o))
      o))


#|
 «tolispytree-tests»  (to ".tolispytree-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("lisptree.lisp");
load("lisptree-middle.lisp");
to_lisp();
               #$[f,2,[g,3,4]]$
  (tolispytree #$[f,2,[g,3,4]]$)
  (to-maxima)

|#


;; «maxima»  (to ".maxima")
;; Functions callable from Maxima.
;;
(defun $tolispytreestr (o)
"This function converts a Maxima tree `o' to a lispy tree converted to a string.
For example, tolispytreestr([f,2,[g,3,4]])
returns                  \"($F 2 ($G 3 4))\".
This is mainly for debugging."
  (format nil "~S" (tolispytree o)))

(defun $tolisptree2d_ (o)
"This function converts a Maxima tree `o' to a 2D tree.
For example, tolispytreestr([f,2,[g,3,4]])
returns (roughly) this:
\"$F__.
 |   |
 2   $G__.
     |   |
     3   4\""
  (lisptree::toplain-lines (lisptree::lispytree (tolispytree o))))

(defun $tolisptree2d (o)
"This is like `$tolisptree2d_', but adds an initial newline."
  (format nil "~%~a" ($tolisptree2d_ o)))

#|
 «maxima-tests»  (to ".maxima-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("lisptree.lisp");
load("lisptree-middle.lisp");

tolispytreestr([f,2,[g,3,4]]);
tolisptree2d_ ([f,2,[g,3,4]]);
tolisptree2d  ([f,2,[g,3,4]]);
tolispytreestr([f, 4, "5", [g, foo, ?foo, Foo]]);
tolisptree2d  ([f, 4, "5", [g, foo, ?foo, Foo]]);

|#



;; Local Variables:
;; coding:  utf-8-unix
;; End:
