;; This file:
;;   http://anggtwu.net/lisptree/vbtbox.lisp.html
;;   http://anggtwu.net/lisptree/vbtbox.lisp
;;          (find-angg "lisptree/vbtbox.lisp")
;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;;
;; This file implements the Lisp part of the "support for vbtboxes" in
;; Lisptree. By default Lisptree returns multiline strings, and its
;; output is like this:
;;
;;   (%i2) lisptree(f(a,b));
;;   (%o2)
;;   f__.
;;   |  |
;;   a  b
;;   (%i3)
;;
;; When Lisptree is configured to use vbtboxes lisptree(f(a,b))
;; returns this,
;;
;;   vbtbox("f__.",
;;          "|  |",
;;          "a  b")
;;


;; (defun e () (interactive) (find-angg "lisptree/vbtbox.lisp"))
;; (defun o () (interactive) (find-angg "lisptree/lisptree-tex.lisp"))
;;
;; «.call-lua»		(to "call-lua")
;; «.lines»		(to "lines")
;; «.lines-tests»	(to "lines-tests")
;; «.lua»		(to "lua")
;; «.maxima»		(to "maxima")
;; «.maxima-tests»	(to "maxima-tests")


;; «call-lua»  (to ".call-lua")
;; Used by: (find-lisptree "vbtbox.mac")
;;    Uses: (find-lisptree "call-lua.c")
;;          (find-lisptree "call-lua.lua")

(load #P"~/quicklisp/setup.lisp")
(ql:quickload :cffi)

;; Loads the .so for:     (find-lisptree "call-lua.c")
;; Generate the .so with: (find-lisptree "call-lua.c" "tests")
(cffi:load-foreign-library    "~/lisptree/call-lua.so")

(defun lua-my-init ()
  (cffi:foreign-funcall "lua_my_init"))
(defun lua-fs (f arg1)
  (cffi:foreign-funcall "lua_fs"  :string f :string arg1 :string))
(defun lua-fss (f arg1 arg2)
  (cffi:foreign-funcall "lua_fss" :string f :string arg1 :string arg2 :string))


;; «lua»  (to ".lua")
;; See: (find-lisptree "call-lua.lua")
;;      (find-lisptree "call-lua.c" "lua_my_init")
;;      (find-es "lisp" "sharpsign-P")
(lua-my-init)
(lua-fs "dofile" (namestring (truename "~/lisptree/vbtbox.lua")))
;; (lua-fs "dofile"           "/home/edrx/lisptree/vbtbox.lua")


;; «lines»  (to ".lines")
;; Convert a string with newlines to a list of lines and vice-versa.
;; See: (find-es "lisp" "substitute")
;;      (find-es "lisp" "split-string")
;;      (find-es "lisp" "cl-str")
(ql:quickload "str")

(defun slash-to-nl   (str) (substitute #\Linefeed #\/ str))
(defun split-lines   (bigstr) (uiop:split-string bigstr :separator uiop:+lf+))
(defun join-as-lines (strings) (str:join #\newline strings))

#|
 «lines-tests»  (to ".lines-tests")
 (eepitch-sbcl)
 (eepitch-kill)
 (eepitch-sbcl)
(load "vbtbox.lisp")
                            (slash-to-nl "a/bc/def")
               (split-lines (slash-to-nl "a/bc/def"))
(join-as-lines (split-lines (slash-to-nl "a/bc/def")))
               ($splitlines (slash-to-nl "a/bc/def"))
($join_as_lines "ab" "cd")

|#




;; «maxima»  (to ".maxima")
;; Calls: (find-lisptree "vbtbox.lua" "verbatim")
(defun $join_as_lines  (&rest strings) (join-as-lines strings))
(defun $splitlines     (bigstr) (cons '(mlist simp) (split-lines bigstr)))
(defun $verbatimbgbox0 (bigstr) (lua-fs "verbatimbgbox0" bigstr))




#|
 «maxima-tests»  (to ".maxima-tests")
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load("vbtbox.lisp");
               join_as_lines("ab", "cd");
    splitlines(join_as_lines("ab", "cd"));
verbatimbgbox0(join_as_lines("ab", "cd"));
verbatimbgbox0("a_b");

|#
