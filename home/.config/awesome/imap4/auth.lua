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
    date created: 4/29/2011

    imap4.lua support module offering auth mechanisms.  At time of creation,
    only cram_md5 is implemented.

--]]

-- table for module exports
local md5 = require("md5")
local mime = require("mime")


local function cram_md5(argt)
    --[[
        Implementation of cram-md5 authentication as described in RFC2195.  The
        technique is based off of the algorithm described in RFC2104.

        This returns a closure using elements of `argt` as the upvalues.  The
        returned function performs the necessary calculations and returns a
        response string for the server response `s_resp` and `nil` to signify no
        further server processing is expected.
    --]]
    return function (s_resp)
               local key = argt['pw']
               local user = argt['user']
               local text = mime.unb64(s_resp)

               if #key > 64 then key = md5.sum(key) end
               if #key < 64 then key = key..string.char(0):rep(64-#key)
               end
               local ixor = md5.exor(key, string.char(54):rep(64))
               local oxor = md5.exor(key, string.char(92):rep(64))
               local digest = md5.sumhexa(oxor..md5.sum(ixor..text))
               return mime.b64(user.." "..digest), nil
           end
end

--[[
    Authentication Table

    This table contains the supported authentication mechanisms.  It consists of
    a mechanism as the key and then a function which takes a table as an
    argument and returns a function that that takes a server response as an
    argument.  No pre-processing of server responses is done prior to calling
    the function.

    The server response processing function should return 2 values: a string to
    be sent as the client response to the server, and a function to respond to
    subsequent server responses.  If none are expected, then return 'nil'.
--]]
local AUTH_T = { 
                ['CRAM-MD5'] = cram_md5
               }

-- handle module exporting 
local M = {}
-- first, a flag as to whether the md5 library is 
if md5 then
    M.md5 = true
else
    M.md5 = false
end
M.mechanims = AUTH_T
return M

