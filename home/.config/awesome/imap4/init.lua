--[[
Copyright (c) <2010> <Gerry LaMontagne>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--]]

--[[
    author: Gerry LaMontagne
    date created: 10/2010

    Handles loading the package so all a package user has to do is use a 
    require("imap4") line or some variant thereof 
    (eg local imaplib = require("imap4"))
--]]

-- define the install path for the library
--local path = "/usr/local/share/lua/5.1/imap4/"
local path = "/home/zack/.config/awesome/imap4/"

-- define a loader function that can load a file from a known spot
local function loader(modulename)
    local filename = path..modulename..".lua"
    -- IMPORTANT: loadfile compiles the code in 'filename' and returns a
    --            function.  In order to *define* the function, it has to
    --            executed- then the module will be available.
    local m = assert(loadfile(filename))
    return m()

end

-- assign preload values for the helper modules that 'imap4' uses, we do this
-- because those modules are not in the module search path for lua since we 
-- decided to put everything under an 'imap4' directory.
package.preload['auth'] = loader
package.preload['utils'] = loader

-- All of this is kicked off by someone using "local imaplib = require("imap4")"
-- in their source code.
-- Preloads and the like don't work because the loader finds *this* file
-- (init.lua) under the directory /imap4, so it is already going to assign
-- package.loaded['imap4'] to this module- which we don't want.  We want it to
-- make the assignment to compiled code of 'imap4.lua'  So we use the loader
-- function above to configure a loader for 'imap4.lua' and then we invoke the
-- function and make the assignment ourselves.
return loader(...)
