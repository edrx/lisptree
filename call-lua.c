// This file:
//   http://anggtwu.net/lisptree/call-lua.c.html
//   http://anggtwu.net/lisptree/call-lua.c
//          (find-angg "lisptree/call-lua.c")
//   Author: Eduardo Ochs <eduardoochs@gmail.com>
//     Date: 2025sep21
//  License: Public Domain
// Based on: (find-angg "LUA/Gdb1.c")
//
// This file implements a way to call Lua from Common Lisp using CFFI.
// The main C functions here are `lua_fs' and `lua_fss', that receive
// strings and return a string, and that work like this:
//
//   lua_fs ("myfun", "aaa")          ->  return tostring(myfun("aaa"))
//   lua_fss("myfun", "aaa", "bbb")   ->  return tostring(myfun("aaa, "bbb""))
//
// The right way to do things would be to make CFFI handle objects of
// the form "lua_State *L", but I am a beginner with Common Lisp, and
// I found that the right way was too hard for me - so instead I
// created a "static lua_State *L", that is initially NULL, but is
// initialized be `lua_my_init'.
//
// I debugged this by modifiyng this file and doing `M-x c', where:
// (defun c   () (interactive) (find-lisptreesh "./call-lua.sh compileandrun"))
// (defun cls () (interactive) (find-lisptree "call-lua.sh"))
// (defun clc () (interactive) (find-lisptree "call-lua.c"))
// (defun cll () (interactive) (find-lisptree "call-lua.lua"))
//
// This file: (find-lisptree "call-lua.c")
//       See: (find-lisptree "call-lua.lua")
//            (find-lisptree "call-lua.sh")
//            (find-lua53manual "#luaL_newstate")
//            (find-lua53manual "#luaL_openlibs")
//            (find-lua53manual "#lua_getglobal")
//            (find-lua53manual "#lua_pop")
//            (find-lua53manual "#lua_pushstring")
//            (find-lua53manual "#lua_pushvalue")
//            (find-lua53manual "#lua_tostring")
//
// «.lua_my_init»	(to "lua_my_init")
// «.lua_fs»		(to "lua_fs")
// «.lua_fss»		(to "lua_fss")
// «.PPstack»		(to "PPstack")
// «.main»		(to "main")
// «.tests»		(to "tests")

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <stdio.h>

static lua_State *L = NULL;

// «lua_my_init»  (to ".lua_my_init")
void lua_my_init(void) {
 if (!L) {
    L = luaL_newstate();
    luaL_openlibs (L);
  }
}

// «lua_fs»  (to ".lua_fs")
// Returns tostring(f(arg1)).
const char *lua_fs(char *f, char *arg1) {
  const char *result;
  lua_getglobal  (L, "tostring");
  lua_getglobal  (L, f);
  lua_pushstring (L, arg1);
  lua_call       (L, 1, 1);
  lua_call       (L, 1, 1);
  result = lua_tostring(L, -1);
  lua_pop(L, 1);
  return result;
}

// «lua_fss»  (to ".lua_fss")
// Returns tostring(f(arg1,arg2)).
const char *lua_fss(char *f, char *arg1, char *arg2) {
  const char *result;
  lua_getglobal  (L, "tostring");
  lua_getglobal  (L, f);
  lua_pushstring (L, arg1);
  lua_pushstring (L, arg2);
  lua_call       (L, 2, 1);
  lua_call       (L, 1, 1);
  result = lua_tostring(L, -1);
  lua_pop(L, 1);
  return result;
}

// «PPstack»  (to ".PPstack")
// I use this function for debugging when I write C functions that
// call the Lua API - it prints all the items in the Lua API stack and
// leaves the stack unchanged. In some cases I set PPstack=PP, where
// PP is this function defined in my init file,
//   (find-angg "LUA/lua50init.lua" "PP")
// and sometimes I use this, that has a prettier output:
//   (find-lisptree "vbtbox.lua" "PPstack")
void PPstack(void) {
  int n = lua_gettop(L);
  int i;
  lua_getglobal   (L, "PPstack");
  for (i=0;i<n;++i)
    lua_pushvalue (L, -n-1);
  lua_call        (L, n, 0);
}

// «main»  (to ".main")
// For tests.
#ifdef MAIN
int main(int argc, const char ** argv) {
  lua_my_init();
  lua_fs ("print",  "In: call-lua.c");
  lua_fss("print",  "In: call-lua.c", "(main)");
  lua_fs ("dofile", "/home/edrx/LUA/lua50init.lua");
  lua_fs ("eval",   "foo = function (a,b) return 10*a+b end");
  lua_fs ("eval",   "PPexpr = function (str) PP(expr(str)) end");
  lua_fs ("PPexpr", "2+3");
  lua_fs ("PPexpr", "foo(2,3)");
  //
  lua_fs ("eval",   "PPstack = PP");
  //
  lua_pushinteger(L, 100);
  lua_pushnumber (L, 200);
  lua_pushstring (L, "300");
  lua_getglobal  (L, "print");
  //
  lua_fs         ("print",  "");
  PPstack        ();
  lua_fs         ("dofile", "vbtbox.lua");   // redefines PPstack
  lua_fs         ("print",  "");
  PPstack        ();
  lua_fs         ("print",  "");
  lua_pop(L, 4);
  PPstack        ();
  //
  return 0;
}
#endif


/* «tests»  (to ".tests")
 * Uses: (find-lisptree "call-lua.sh")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
./call-lua.sh compile
ls -laF call-lua*
./call-lua

 (eepitch-sbcl)
 (eepitch-kill)
 (eepitch-sbcl)
(load #P"~/quicklisp/setup.lisp")
(ql:quickload :cffi)
(cffi:load-foreign-library "./call-lua.so")

(defun lua-my-init ()
  (cffi:foreign-funcall "lua_my_init"))
(defun lua-fs (f arg1)
  (cffi:foreign-funcall "lua_fs"  :string f :string arg1 :string))
(defun lua-fss (f arg1 arg2)
  (cffi:foreign-funcall "lua_fss" :string f :string arg1 :string arg2 :string))

;; Rewrite this part:
(lua-my-init)
(lua-fs  "dofile"    "./call-lua.lua")
;; See: (find-lisptree "call-lua.lua" "requires")
;;      (find-lisptree "call-lua.lua" "verbatim")
(lua-fs  "verbatimbgbox" "a__b")
(lua-fs  "verbatimbgbox" "a__.
|  |
b  c")

*/


// Local Variables:
// coding:  utf-8-unix
// End:
