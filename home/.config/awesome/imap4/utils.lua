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
    date created: 4/29/2010

    Module to hole miscellaneous support modules for luaimap4.

--]]

local table = require("table")

local function keys(t)
    --[[
        Returns table of non-numeric key values in a table.

        Arguments:
            t:  any table

        Returns:
            Table of non-numeric(index?) key values
    --]]
    if not t then return nil end
    local key_t = {}
    for k in pairs(t) do 
        if type(k) ~= 'number' then 
            table.insert(key_t, k) 
        end
    end
    return key_t
end

local function issubset(t1, t2)
    --[[
        Return true if t1 is a subset of t2, works only for indexed tables

        Arguments:
            t1:  indexed table
            t2:  indexed table that establishes the set

        Returns:
            true if t1 is a subset of t2, false otherwise
    --]]
    if not t1 then return true end
    local set_t = {}
    for i,v in ipairs(t2) do set_t[v] = true end
    for i,v in ipairs(t1) do 
        if not set_t[v] then return false end
    end
    return true
end

local function makeordered(t, order)
    --[[
        Returns a table whose values come from a key-value table and in a
        specified order.

        Arguments:
            t: a key-value based table

            order: a table which lists the keys of table 't' in the order they
                   should be inserted into the return table

        Returns:
            Indexed table whose values are those of table 't' and is ordered
            according to the key order specified in 'order' or nil if t is nil
    --]]
    if not t then return t end
    local new_t = {}
    for i,v in ipairs(order) do
        if t[v] then table.insert(new_t, t[v]) end
    end
    if #new_t ~= 0 then 
        return new_t 
    else
        return t
    end
end

local function arg_error(argt, literals)
    local err_msg = ''
    local literal_i = 1
    for i,v in ipairs(argt) do
        if v[1] == ''  then
            if literals and not literals[literal_i] then
                err_msg = err_msg..v[1]..CRLF
            else
                literal_i = literal_i + 1
            end
        end
    end
    error(err_msg)
end

local M = {}
M.keys = keys
M.issubset = issubset
M.makeordered = makeordered
M.arg_error = arg_error
return M
