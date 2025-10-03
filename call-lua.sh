#!/bin/bash
# This file:
#   http://anggtwu.net/lisptree/call-lua.sh.html
#   http://anggtwu.net/lisptree/call-lua.sh
#          (find-angg "lisptree/call-lua.sh")
# Author: Eduardo Ochs <eduardoochs@gmail.com>
#
# This script help(ed) me to debug this:
#   (find-lisptree "call-lua.c" "main")





LUA_INCLUDE_DIR=/usr/include/lua5.3
LUA_LIB=lua5.3
STEM=call-lua

cd ~/lisptree/

compile () {
  gcc -gdwarf-4 -g3 -shared -I${LUA_INCLUDE_DIR} -o ${STEM}.so ${STEM}.c -l${LUA_LIB}
  gcc -gdwarf-4 -g3 -DMAIN  -I${LUA_INCLUDE_DIR} -o ${STEM}    ${STEM}.c -l${LUA_LIB}
}
compileandrun () {
  gcc -gdwarf-4 -g3 -DMAIN  -I${LUA_INCLUDE_DIR} -o ${STEM}    ${STEM}.c -l${LUA_LIB} &&
  ./${STEM}
}

eval $*


: <<'%%%%%'
 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
./call-lua.sh compileandrun

%%%%%



# Local Variables:
# coding:  utf-8-unix
# End:
