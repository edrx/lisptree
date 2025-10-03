-- This file:
--   http://anggtwu.net/lisptree/vbtbox.lua.html
--   http://anggtwu.net/lisptree/vbtbox.lua
--          (find-angg "lisptree/vbtbox.lua")
-- Author: Eduardo Ochs <eduardoochs@gmail.com>
--
-- (defun e () (interactive) (find-angg "lisptree/vbtbox.lua"))

--
-- See: (find-lisptree "call-lua.c")
-- Based on: (find-angg "LUA/Gdb1.lua")
--
-- «.edrxlib»		(to "edrxlib")
-- «.addLUAtopath»	(to "addLUAtopath")
-- «.PPstack»		(to "PPstack")
-- «.requires»		(to "requires")
-- «.verbatim»		(to "verbatim")


-- «edrxlib»  (to ".edrxlib")
-- See: (find-angg "LUA/lua50init.lua")
-- See: (find-angg "LUA/lua50init.lua" "edrxlib")
if not package.loaded.edrxlib then
  dofile((os.getenv"LUA_INIT"):sub(2))
end


-- «PPstack»  (to ".PPstack")
-- See: (find-lisptree "call-lua.c" "PPstack")
--      (find-lisptree "call-lua.c" "main")
PPstack = function (...)
    local L = {...}
    if #L == 0 then print("Empty stack"); return end
    for i=1,#L do
      print(format("%s/%s: %s", i, i-#L-1, mytostring(L[i])))
    end
  end



-- «addLUAtopath»  (to ".addLUAtopath")
-- See: (find-angg "LUA/lua50init.lua" "Path" "addLUAtopath =")
--      (find-angg "LUA/lua50init.lua" "Path.addLUAtopath")
Path.addLUAtopath()


-- «requires»  (to ".requires")
-- These `require's have lots of dependencies (in ~/LUA/).
require "Verbatim4"    -- (find-angg "LUA/Verbatim3.lua")
--require "CME3"       -- (find-angg "LUA/CME3.lua")


-- «verbatim»  (to ".verbatim")
-- See: (find-lisptree "vbtbox.lisp" "maxima")
--      (find-angg "LUA/Verbatim3.lua")
--      (find-angg "LUA/Verbatim3.lua" "Verbatim")
--      (find-angg "LUA/Verbatim3.lua" "Verbatim-tests")

Verbatim.__index._uncomment = function (vb)
    vb.o = vb.o:gsub("%%\n *", "")
  end
Verbatim.__index._bgbox = function (vb)
     vb:_fmt("\\myvcenter{\\vbtbgbox{%s}}", vb.o)
  end

verbatimbgbox0 = function (bigstr)
    local vb = Verbatim.from(splitlines(bigstr))
    return vb:act("e h c p v uncomment bgbox").o
  end


--[[
-- (sbcp 1 "title")
-- (sbca   "title")
 (eepitch-lua51)
 (eepitch-kill)
 (eepitch-lua51)
dofile "vbtbox.lua"
-- dofile "CME3.lua"

str0 = "a__.\n|  |\nb  c"
vb = Verbatim.from(splitlines(str0))
= vb
= vb:act("e h c p v uncomment bgbox")

= verbatimbgbox(str0)


--]]






-- (defun e () (interactive) (find-angg "lisptree/call-lua.lua"))

-- Local Variables:
-- coding:  utf-8-unix
-- End:
