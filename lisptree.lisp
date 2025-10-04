;; This file:
;;   https://anggtwu.net/luatree/lisptree.lisp.html
;;   https://anggtwu.net/luatree/lisptree.lisp
;;           (find-angg "luatree/lisptree.lisp")
;;    See:   https://anggtwu.net/lisptree.html  <- has screenshots!
;;       https://github.com/edrx/lisptree
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;; License: Public Domain
;; Date: 2025oct04
;;
;; This is the inner part of LispTree.
;; The other parts are here:
;;   middle: (find-lisptree "lisptree-middle.lisp")
;;    outer: (find-lisptree "lisptree.mac")
;; See the "introduction" below.
;;
;; This was inspired by this Emacs package:
;;   https://anggtwu.net/show-conses.html
;; but show-conses supports text properties and this doesn't.
;;
;; «.introduction»		(to "introduction")
;; «.introduction-tests»	(to "introduction-tests")
;; «.package»			(to "package")
;; «.mapconcat»			(to "mapconcat")
;; «.mapconcat-tests»		(to "mapconcat-tests")
;; «.toplain»			(to "toplain")
;; «.toplain-tests»		(to "toplain-tests")
;; «.width»			(to "width")
;; «.width-tests»		(to "width-tests")
;; «.pad»			(to "pad")
;; «.pad-tests»			(to "pad-tests")
;; «.lr»			(to "lr")
;; «.lr-tests»			(to "lr-tests")
;; «.lispytree»			(to "lispytree")
;; «.lispytree-tests»		(to "lispytree-tests")

;; «introduction»  (to ".introduction")
;; Lisptree draws Maxima objects as 2D trees, like this:
;;
;;                     f__.
;;                     |  |
;;   f(a,g(b,c))  ==>  a  g__.
;;                        |  |
;;                        b  c
;;
;; This file - lisptree.lisp - does a part of the conversion. The
;; top-level functions here are `lispytree' and `toplain-lines', and
;; they work like this:
;;
;;                                    (("2" "__" ".")          
;; 				       ("|" "  " "|")          
;;   (lispytree '(2 3 (4 5 6)))  ==>   ("3" "  " "4" "__" ".") 
;; 				          ("   " "|" "  " "|") 
;; 				          ("   " "5" "  " "6"))
;;
;;                                                    "2__.\n"    ..
;;                                                    "|  |\n"    ..
;;   (toplain-lines (lispytree '(2 3 (4 5 6))))  ==>  "3  4__.\n" ..
;;                                                    "   |  |\n" ..
;;                                                    "   5  6"     
;;
;; Here's a smaller example:
;;                 
;;                    (("4" "__" ".")         "4__.\n" ..
;;      (4 5 6)  ==>   ("|" "  " "|")   ==>   "|  |\n" ..
;;                     ("5" "  " "6"))	      "5  6"     
;;
;;  A Lispy tree -->   a LINES object   --> a string with newlines

#|
 «introduction-tests»  (to ".introduction-tests")
 (eepitch-sbcl)
 (eepitch-kill)
 (eepitch-sbcl)
(load "lisptree.lisp")
(in-package :lisptree)
               (lispytree '(2 3 (4 5 6)))
(toplain-lines (lispytree '(2 3 (4 5 6))))
(toplain-lines (lispytree '(2 3 (4 5 "6"))))
               (lispytree '("f" "a" ("g" "b" "c")))
(toplain-lines (lispytree '("f" "a" ("g" "b" "c"))))

|#




;; «package»  (to ".package")
;; The functions in this file are put in the package `:lisptree'.
;; The functions in lisptree-middle.lisp are put in the package `:maxima'.
;; See: (find-lisptree "lisptree-middle.lisp" "package")
;;
(defpackage :lisptree (:use :common-lisp))
(in-package :lisptree)


;; «mapconcat»  (to ".mapconcat")
;;
(defvar newline (format nil "~%"))

(defun concat (&rest strs)
  (apply 'concatenate 'string strs))

(defun myconcat (strings sep)
  (let* ((firststring  (car strings))
	 (otherstrings (cdr strings))
         (pairs        (loop for string in otherstrings
                             collect sep
                             collect string))
         (items (cons firststring pairs)))
    (apply 'concatenate 'string items)))

(defun mapconcat (f list sep)
  (myconcat (map 'list f list) sep))

#| 
 «mapconcat-tests»  (to ".mapconcat-tests")
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
(defun f (o) (format nil "[~a]" o))
(f 42)                               ; -> "[42]"
(mapconcat #'f '(2 3 4) "_")         ; -> "[2]_[3]_[4]"
(mapconcat #'f '(2 3 4) newline)     ; -> "[2]\n[3]\n[4]"

|#


;; «toplain»  (to ".toplain")
;; `toplain-lines' converts a LINES object to a string with newlines.
;; This functions is based on the `toplain' functions from
;; show-conses-el - but there the "toplain" meant "convert to plain
;; text, discarding all the information about text properties".
;;
(defun toplain-line (line)
  (apply 'concat line))

(defun toplain-lines (lines)
  (mapconcat 'toplain-line lines newline))

(defvar demo-lines-1)
(setq   demo-lines-1
	'(("a" "__" "b")
	  ("|")
	  ("c")))

#|
 «toplain-tests»  (to ".toplain-tests")
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
                   demo-lines-1     ; -> (("a" "__" "b") ("|") ("c"))
              (car demo-lines-1)    ; ->  ("a" "__" "b")
(toplain-line (car demo-lines-1))   ; -> "a__b"
(toplain-lines     demo-lines-1)    ; -> "a__b\n|\nc"

|#


;; «width»  (to ".width")
;;
(defun width-line (line)
  (length (toplain-line line)))

(defun widths-of-lines (lines)
  (mapcar 'width-line lines))

(defun width-lines (lines)
  (apply 'max (widths-of-lines lines)))

#|
 «width-tests»  (to ".width-tests")
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
                 demo-lines-1    ; -> (("a" "__" "b") ("|") ("c"))
(widths-of-lines demo-lines-1)   ; -> (4 1 1)
    (width-lines demo-lines-1)   ; -> 4

|#


;; «pad»  (to ".pad")
;; A LINES object can be like this,
;;   (("a" "__" "b") ("|") ("c"))
;; and have lines with different widths. When we apply the function
;; `pad-lines' on the LINES object above it returns this,
;;   (("a" "__" "b") ("|" "   ") ("c" "   "))
;; that is a LINES object in which all lines have the same width.
;;
(defun pad-line (wtotal line &optional char)
  "Pad LINE to the width WTOTAL."
  (let* ((wleft  (width-line line))
	 (wright (- wtotal wleft))
	 (spaces (make-string wright :initial-element (or char #\ ))))
    (if (< wleft wtotal)
	(append line (list spaces))	; add spaces at the right if needed
      line)))				; or return LINE unchanged

(defun pad-lines (lines)
  (let ((maxwidth (width-lines lines)))
    (loop for line in lines
          collect (pad-line maxwidth line))))

#|
 «pad-tests»  (to ".pad-tests")
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
           demo-lines-1    ; -> (("a" "__" "b") ("|") ("c"))
(pad-lines demo-lines-1)   ; -> (("a" "__" "b") ("|" "   ") ("c" "   "))

|#



;; «lr»  (to ".lr")
;; Join two LINES objects by drawing one at the left of the other.
;; Suppose that `o1' and `o2' are:
;;
;;        (("a")                 (("gh")
;;   o1 =  ("bc")     and   o2 =  ("i"))
;;         ("def"))
;;
;; and that o3 is `(add-pin '(".") o2)'. Then we have this, if we draw
;; the LINES objects without the quotes and parentheses:
;;
;;        a          gh        .
;;   o1 = bc    o2 = i    o3 = |
;;        def                  gh
;;                             i
;;
;;                a  gh                 a____gh
;;   (lr o1 o2) = bc i	  (l_r o1 o2) = bc   i
;;                def                   def
;;
;;                                      a____.
;;                   	  (l_r o1 o3) = bc   |
;;                                      def  gh
;;                                           i
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
 «lr-tests»  (to ".lr-tests")
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(in-package :lisptree)
(defvar o1)
(defvar o2)
(defvar o3)
(setq o1 '(("a") ("bc") ("def")))
(setq o2 '(("gh") ("i")))
(setq o3 (add-pin '(".") o2))
(lr  o1 o2)
(l_r o1 o2)
(l_r o1 o3)
(toplain-lines (lr  o1 o2))
(toplain-lines (l_r o1 o2))
(toplain-lines (l_r o1 o3))

|#



;; «lispytree»  (to ".lispytree")
;; See the figures in the introduction at the top of this file.
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
 «lispytree-tests»  (to ".lispytree-tests")
 (eepitch-sly)
 (eepitch-kill)
 (eepitch-sly)
(load "lisptree.lisp")
(toplain-lines (lispytree '("f" "a" ("g" "b" "c"))))
(toplain-lines (lispytree '(2 3 (4 5 "6"))))

(lispytree-atom   "*")
(lispytree-atom   " ")
(lispytree-opargs "*" '(2 3))
(lispytree-opargs "*" '(2))
(lispytree-opargs "*" '())

(lispytree 2)
(lispytree '(2 3))
(lispytree '(2 3 4))
               (lispytree '(2 3 (4 5 6)))
(toplain-lines (lispytree '(2 3 (4 5 6))))
(toplain-lines (lispytree '(2 3 (4 5 "6"))))
               (lispytree '("f" "a" ("g" "b" "c")))
(toplain-lines (lispytree '("f" "a" ("g" "b" "c"))))

|#




;; Local Variables:
;; coding:  utf-8-unix
;; End:
