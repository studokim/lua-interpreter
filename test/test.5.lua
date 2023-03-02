if 0 then a00 = 1 else a00 = 0 end
if 1 then a01 = 1 else a01 = 0 end
if (0 + 1) then a02 = 1 else a02 = 0 end

if "" then a10 = 1 else a10 = 0 end
if "str" then a11 = 1 else a11 = 0 end

if x then a20 = 1 else a20 = 0 end
x = 1

if f then a30 = 1 else a30 = 0 end
function f() end
if f then a31 = 1 else a31 = 0 end

y = false
__show_env()
