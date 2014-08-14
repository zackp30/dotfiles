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

    Conventions: 
    Seeing as this is a lua program, there are really no "private" entities in
    objects.  Everything within the object is available for manipulation by the
    programmer.  However, methods and variables that are not intended for public
    usage are prefixed with a "__".

    All objects are created using a "new" method.

    All objects names start with a capital letter.  Object methods use all
    lower-case with underscores separating words.

    In general, if an IMAP4 command is supported, it will be a lower-case method
    of an IMAP4 object.  For example, assuming an IMAP4 object 'imap,' IMAP4
    commands 'LOGIN', 'NOOP', and 'CAPABILITY' are as follows:

        imap.login('user', 'secret')
        imap.noop()
        imap.capability()

    DISCLAIMER:
    This library is intended to take care of the drudgery involved in
    interacting with an IMAP server.  It makes no attempt to enforce coding or
    design practices or protect a using program from itself.  So, for example,
    if you decide to login over an unsecured channel using this library, any
    problems that arise as a result are on you, the developer of said program.

    Anyone who uses this library should take the time to read the RFC3501 spec
    to understand exactly what these commands do and  what the recommendations
    are when considering designing an IMAP client.

--]]

-- create local environment
local table = require("table")
local string = require("string")
local socket = require("socket")
if not socket then
    error([[Unable to import socket library.  Imaplib requires lua-socket-
            please make sure it is installed and visible to the lua
            interpreter.]])
end
local ssl = require("ssl")
local auth = require("auth")
local utils = require("utils")

--local __debug__ = print
local __debug__ = function() end

--[[

    CONSTANTS

--]]
local CRLF = "\r\n"
local IMAP4_port = 143
local IMAP4_SSL_port = 993

local IMAP4_states = { "NOT_AUTHENTICATED", "AUTHENTICATED", "SELECTED", "LOGOUT"}
local NONAUTH = IMAP4_states[1]
local AUTH = IMAP4_states[2]
local SELECT = IMAP4_states[3]
local LOGOUT = IMAP4_states[4]

local VALID_RESP_TOKENS = { ['+'] = true, ['*'] = true }
local VALID_RESPONSE_CODES = {
                              ALERT = true,
                              BADCHARSET = true,
                              CAPABILITY = true,
                              PARSE = true,
                              PERMANENTFLAGS = true,
                              ['READ-ONLY'] = true,
                              ['READ-WRITE'] = true,
                              TRYCREATE = true,
                              UIDNEXT = true,
                              UIDVALIDITY = true,
                              UNSEEN = true,
                             }
local RESP_COMPLETION = { OK = true, NO = true, BAD = true }

local UNIVERSAL = { [NONAUTH] = 1, [AUTH] = 1, [SELECT] = 1, [LOGOUT] = 1 }
local NOT_AUTHENTICATED = { [NONAUTH] = 1 }
local AUTHENTICATED = { [AUTH] = 1, [SELECT] = 1}
local SELECTED = { [SELECT] = 1 }
local ALLOWED_STATES = {
                        APPEND = AUTHENTICATED,
                        AUTHENTICATE = NOT_AUTHENTICATED,
                        CAPABILITY = UNIVERSAL,
                        CHECK = SELECTED,
                        CLOSE = SELECTED,
                        COPY = SELECTED,
                        CREATE = AUTHENTICATED,
                        DELETE = AUTHENTICATED,
                        EXAMINE = AUTHENTICATED,
                        EXPUNGE = SELECTED,
                        FETCH = SELECTED,
                        LIST = AUTHENTICATED,
                        LOGIN = NOT_AUTHENTICATED,
                        LOGOUT = UNIVERSAL,
                        LSUB = AUTHENTICATED,
                        NOOP = UNIVERSAL,
                        RENAME = AUTHENTICATED,
                        SEARCH = SELECTED,
                        SELECT = AUTHENTICATED,
                        STARTTLS = NOT_AUTHENTICATED,
                        STATUS = AUTHENTICATED,
                        STORE = SELECTED,
                        SUBSCRIBE = AUTHENTICATED,
                        UID = SELECTED,
                        UNSUBSCRBE = AUTHENTICATED,
                       }

-- String Patterns
local TAG_PAT = "%a%d+"
local RESPONSE_UNTAGGED = "^(%*) "
local RESPONSE_CONTINUE = "^(%+) "
local RESPONSE_TAGGED = "^(%a%d+) "
local RESPONSE_TYPES_PAT = {RESPONSE_UNTAGGED, RESPONSE_CONTINUE, RESPONSE_TAGGED}
local RESPONSE_PAT1 = "^(%u+) (.*)\r\n"
local RESPONSE_PAT2 = "^(%d+) (%u+) ?(.*)\r\n"

--[[
    Response Object

    An object for holding and accessing responses from an IMAP server.

    tagged responses-   an indexed table with the tag, result and text as seperate
                        entities within the table.  
    
    untagged responses- a keyed table with the initial atom of the response used
                        as the key.

    response codes- these are a portion of either a tagged or untagged response
                    that appear in square brackets.  It is provided as a
                    courtesy to the using program so that response content can
                    be more quickly parsed if expecting certain information.
--]]
local Response = {}
Response.__index = Response  -- make table usable as an object

function Response.__print(self)
    print("RESPONSES:")
    if next(self.__tagged) then
        print("\tTAGGED:")
        for k,v in pairs(self.__tagged) do
            print("\t\t"..k..":  "..v)
        end
    end
    if next(self.__untagged) then
    print("\tUNTAGGED:")
        for k,v in pairs(self.__untagged) do
            local resp = ''
            for i,s in ipairs(v) do
                if s[1] ~= '' then
                    resp = resp..s[1].."\r\n\t\t\t"    
                end
                if s[2] then
                    resp = resp..s[2].."\r\n\t\t\t"
                end
            end
            print("\t\t"..k..":\t"..resp)
        end
    end
    if next(self.__codes) then
        print("\tCODES:")
        for k,v in pairs(self.__codes) do
            local resp = ''
            for i,s in ipairs(v) do
                resp = resp..s.."\r\n\t\t\t"
            end
            print("\t\t"..k..":\t"..resp)
        end
    end
end

function Response.__check_response_codes(self, content)
    local ct = self.__codes
    local rcode, data = content:match("%[([A-Z-]+) ?(.*)%]")
    if rcode and VALID_RESPONSE_CODES[rcode] then
        if not ct[rcode] then ct[rcode] = {} end
        table.insert(ct[rcode], data)
    end
end

function Response.__getitem(self, t, i)
    local ret_t = {}
    for i,m in ipairs(t) do table.insert(ret_t, m[i]) end
    return ret_t
end

--[[
    PUBLIC METHODS
--]]
function Response.add_tagged(self, tag, data)
    --[[
        For adding tagged responses to the response object- not needed by a
        Response object user.

        arguments:
        tag:  tag of tagged response
        data:  the data portion of the response, includes result code and other 
               content

        returns: nothing
    --]]
    local result, content = data:match(RESPONSE_PAT1)
    if not content then content = '' end
    self.__tagged['tag'] = tag
    self.__tagged['result'] = result
    self.__tagged['content'] = content
    self:__check_response_codes(content)
end

function Response.add_untagged(self, data)
    --[[
        For adding untagged responses from the server to a response object.
        Untagged responses can be a result of a command, or responses read from
        the line which were unsolicited.

        arguments:
        data:  response string for parsing

        returns: nothing
    --]]
    __debug__("Response.add_untagged:")
    __debug__(data)
    local ut = self.__untagged
    local msg_num, typ, content = data:match(RESPONSE_PAT2)
    if not msg_num then
        typ, content = data:match(RESPONSE_PAT1)
    end
    if not ut[typ] then ut[typ] = {} end
    if not content then content = '' end
    table.insert(ut[typ], { content, msg_num })
    self.__last_untagged = typ
    self:__check_response_codes(content)
end

function Response.add_continuation(self, data)
    --[[
        Appends continuation data received from the server to a continuation
        string.
    --]]
    self.__continuation = self.__continuation..data
end

function Response.add_literal(self, data)
    local t = self.__untagged[self.__last_untagged] 
    t = t[#t]
    t[1] = t[1]..CRLF..data
end

function Response.getContinuation(self)
    return self.__continuation
end

function Response.getTaggedTag(self)
    --[[
        Returns the tag responded to be the server.

        Arguments: none

        Returns:  The actual tag received from the server in response to a
                  tagged command to the server.
    --]]
    return self.__tagged.tag
end

function Response.getTaggedResult(self)
    --[[
        For getting the result code of the tagged server response.

        Arguments: none
        
        Returns:  The result code for a tagged command, typically OK, BAD, NO
    --]]
    return self.__tagged.result
end

function Response.getTaggedContents(self)
    --[[
        Returns content of a tagged response- basically everything after the
        result code.

        Arguments: none

        Returns:  All tagged response content following the result code.  This
                  is typically something about the command completed, or why it
                  failed or what error occurred.  This will also contain
                  bracketed response codes.
    --]]
    return self.__tagged.content
end

function Response.getResponseCodes(self)
    --[[
        Returns all response codes from both tagged and untagged response.

        Arguments: none

        Returns:  A table with all IMAP4Rev1 defined response codes received in
                  the tagged and untagged responses.  Server implementation
                  dependent response codes are not included here.
    --]]
    if self.__codes then
        return utils.keys(self.__codes)
    else
        return nil
    end
end

function Response.getResponseCodeContent(self, code)
    --[[
        Returns the content associated with a response code.

        Arguemnts: the response code whose content to look up

        Return: the content corresponding to the supplied code, or nil if the
                code is not in this response.
    --]]
    if self.__codes[code] then
        return self.__codes[code]
    else
        return nil
    end
end

function Response.getUntaggedResponse(self)
    --[[
        Returns all untagged responses

        Arguments:  none

        Return:  A table of with all response codes received in the response.
    --]]
    if next(self.__untagged) then
        return utils.keys(self.__untagged)
    else
        return nil
    end
end

function Response.getUntaggedContent(self, typ)
    --[[
        Returns all content for specified untagged response code.

        Arguments: untagged response type, such as OK, or LIST

        Returns: a table containing all responses corresponding to the supplied
                 type
    --]]
    if not self.__untagged[typ] then
        return nil
    end
    local ret_t = {}
    for i,v in ipairs(self.__untagged[typ]) do
        if v[1] == '' and v[2] ~= nil then
            table.insert(ret_t, v[2])
        elseif v[2] ~= nil then
            table.insert(ret_t, v)
        else 
            table.insert(ret_t, v[1])
        end
    end
    return ret_t
end

function Response.getUntaggedNum(self, typ)
    --[[
        Returns a numeric value associated with certain response codes.

        Arguments: untagged response type, such as OK or LIST

        Returns: The number corresponding to the untagged response type.  For
                 example, if the type is EXISTS the number returned is the
                 number of existing messages in a mailbox.
    --]]
    if self.__untagged[typ][2] then  
        return self.__untagged[typ][2]
    else
        return nil
    end
end

function Response.new(self)
    -- create the object
    local o = {}
    setmetatable(o, self)

    -- now do some initialization
    o.__tagged = {}
    o.__untagged = {}
    o.__codes = {}
    o.__continuation = ''

    return o
end




--[[
    IMAP4 Object

    Handles command-response exchanges with an IMAP4Rev1 server (RFC3501).  The
    main goal is to provide an interface so the drudgrey of the under-the-hood
    related IMAP protocol is handled but still gives an app developer the
    control to deal with responses as they see fit.

    This code makes no attempt to enforce good client design practices.  It
    is merely meant to serve as a starting point.  Outside of dropped
    connections and properly formed commands to and from the server, error
    handling and IMAP4Rev1 best practices are left to the user of this library.

    To create an object, use IMAP4.new(hostname, [port]).  Then, use the object
    to transmit commands to the server.  For instance, a 'NOOP' command is sent
    using the 'noop' method:
    
        imap = IMAP4:new('my.mail.server')
        imap:noop()

    All IMAP4Rev1 command methods are case insensitive.  So the `SELECT` command
    can be invoked using either of the following forms:

        imap:select()
          - or -
        imap:SELECT()

    All command methods return a Response object.  This object contains all
    responses from the server up to the tag completion response.  So server
    initiated responses will be buried in the "untagged" portion of the
    Response object.

    Commands which require arguments all support the IMAP literal form.  In
    fact, it is possible to mix and match the named arguments with literal ones.
    Commands which require arguments name those arguments for the simplest usage.
    Alternatively, those arguments can be placed into a table and passed into
    the method using the `literals` argument.  The IMAP4 object will handle the
    rest.

    If mixing named and literal arguments, the rule is simple- literals are used
    sequentially for each required named command.  For example, it is possible
    to login as follows:
     
        imap:login('', 'secret', { 'user' })

    The 'user' argument will be sent in its literal form and the password
    argument will be sent in string form.

    Use of literals allows for more character support in command arguments (the
    space character is a good example).  The cost is in extra line turn around,
    since the server has to be told how many bytes to expect, and those bytes
    cannot be sent until the server is ready for them.  Thus the above login
    example takes 2 client transmissions.  Essentially, each literal requires
    it's own transmission.

    Literals can be supplied in two ways- an indexed table or a key-value table.
    The key-value table is internally converted to an indexed table where values
    are placed in the appropriate order for the command.  For commands that have
    optional arguments, you SHOULD use the key-value form as the command results
    cannot be guaranteed if the indexed form is used in these cases.

    For the key-value form of the literal table, the keys MUST be the same as
    the named argument equivalent.  So, for example the `login` function is
    defined with arguments `user` and `pw`.  So the command would look like:

        imap:login(nil, nil, {user = 'me', pw = 'secret'})

--]]
local IMAP4 = {}
local RECEIVE_SIZE = 4096
local MAX_PIPELINE_SIZE = 1000
local NOWAIT = 1
-- The following makes IMAP4 table usable as an object and allows for using the 
-- IMAP methods as all caps. 
-- For example: 
--     imap:NOOP() is mapped to imap:noop().
IMAP4.__index = function (t,k)
                    if ALLOWED_STATES[k] then
                        return IMAP4[k:lower()]
                    else
                        return IMAP4[k]
                    end
                end
--[[
    
    PRIVATE METHODS

--]]
function IMAP4.__open_connection(self)
    self.__connection = assert(socket.connect(self.host, self.port), 
                               'Unable to establish connection with host.\r\n')
    if self.__sslparams.protocol ~= 'tlsv1' then
        self.__connection = ssl.wrap(self.__connection, self.__sslparams)
        assert(self.__connection:dohandshake())
    end
    self.__connection:settimeout(0)
end

function IMAP4.__get_type(self, s)
    for i, re in ipairs(RESPONSE_TYPES_PAT) do
        local m = s:match(re)
        if m then
            -- The 2 below is needed because indexing starts at 1, plus a 
            -- ' ' always follows the response identifier contained in m
            s = s:sub(2+#m, -1)
            return m, s
        end
    end
    error("Invalid server response: " .. s)
end

function IMAP4.__add_literal(self)
    return string.format("{%u}", #self.__literals[1])
end

function IMAP4.__next_tag(self)
    self.__tag_num = self.__tag_num + 1
    return string.format("%s%04u", self.__tagpre, self.__tag_num)
end

function IMAP4.__new_response(self)
    self.__current_response = Response:new()
    table.insert(self.__responses, self.__current_response)
    return self.__current_response
end

function IMAP4.__receive(self, flags)
    --[[
        Buffers data in `__received_data` and returns as soon as data is
        available.
    --]]
    while true do
        local data, emsg, partial = self.__connection:receive(RECEIVE_SIZE)
        if data then self.__received_data = self.__received_data..data end
        if partial then self.__received_data = self.__received_data..partial end
        if emsg == 'timeout' or emsg == 'wantread' then 
            if #self.__received_data ~= 0 or flags == NOWAIT then
                return
            else
                -- rather than spinning our wheels, wait for socket status to
                -- change
                socket.select({ self.__connection }, nil)
            end
        elseif emsg == 'closed' then
            if #self.__received_data == 0 then
                error('Remote closed connection unexpectedly.')
            end
            return
        end
    end
end

function IMAP4.__read(self, cnt)
    --[[
        Reads `cnt` bytes from the received buffer.  If the request is for more
        than is available, read until enough is available to satisfy the
        request.  Otherwise, pulls out `cnt` bytes from the buffer to return and
        adjusts the buffer to reflect the removed bytes
    --]]
    -- add 1 because index starts at 1, plus 2 more to get trailing CRLF
    cnt = cnt + 3
    while #self.__received_data <= cnt do
        self:__receive()
    end
    local ret_str = self.__received_data:sub(1, cnt)
    self.__received_data = self.__received_data:sub(cnt + 1, -1)
    return ret_str
end

function IMAP4.__readline(self, flags)
    --[[
        Return a CRLF terminated string from the receive buffer.  If no CRLF
        exists in the buffer, then read until there is.
    --]]
    local line = ''
    if #self.__received_data == 0 or not self.__received_data:find(CRLF) then
        self:__receive(flags)
    end

    -- check if there is data to return
    if #self.__received_data == 0 then 
        return nil
    else
        -- IMAP4 spec states all responses must end in CRLF
        line, self.__received_data = self.__received_data:match("(.-\r\n)(.*)")
        return line
    end
end

function IMAP4.__get_response(self, flags)
    __debug__("__get_response")
    local resp = self:__readline(flags)
    if not resp then return nil 
    else return self:__get_type(resp)
    end
end

function IMAP4.__buildcmd(self, cmd, args)
    local cmdstr = ''
    if args then cmdstr = cmd..args
    else cmdstr = cmd end
    return cmdstr..CRLF
end

function IMAP4.__handle_argflag(self, arg, flag)
    if flag == 'l' then 
        table.insert(self.__literals, arg)
        return nil
    elseif flag == '()' then 
        arg = '('..arg..')' 
    elseif flag == 'C' then
        arg = 'CHARSET '..arg
    -- these are atom-special characters as defined by RFC3501,
    -- if any of them are in the arg string,then quote the string
    elseif flag ~= "NQ" and
           not arg:find([[^".*"$]]) and 
           arg:find("[ {\"%c%(%)%%%*%]]") then
        arg = '"'..arg..'"'
    end

    return arg
end

function IMAP4.__send_command(self, tag, cmd, args)
    self.__tags[tag] = self:__buildcmd(cmd, args)
    assert(self.__connection:send(tag..' '..self.__tags[tag]))
end

function IMAP4.__send_continuation(self)
    local out
    if self.__literal_func then
        out, self.__literal_func =
                  self.__literal_func(self.__current_response:getContinuation())
    else
        local arg
        local flag
        out = self.__literals[1]  -- this is the literal we've already informed 
                                  -- the server about
        table.remove(self.__literals, 1)

        -- since we allow for mixing of literals and named arguments, we need
        -- to check if anything follows this literal and if so, append those
        -- arguments,  it's also possible that more literals follow this one
        if #self.__argt ~= 0 then
            -- clean up the __argt of optional entries
            repeat
                arg = self.__argt[1][1]
                flag = self.__argt[1][3]
                table.remove(self.__argt, 1)
            until arg ~= ' ' 
            if arg ~= '' then
                arg = self:__handle_argflag(arg, flag)
                if arg then out = out.." "..arg end
            end
        end
        -- be sure to add length of next literal if there are subsequent
        -- literals to send
        if #self.__literals ~= 0 then out = out.." "..self:__add_literal() end
    end
    assert(self.__connection:send(out..CRLF))
end

function IMAP4.__response_handler(self, flags)
    -- this isn't obvious- but r_type can return 1 of 4 values, the three
    -- explicity checked for here plus 'nil' if get_response doesn't return
    -- anything useful
    local r_type, s = self:__get_response(flags)
    if r_type == '*' then
        -- untagged response
        self.__current_response:add_untagged(s)
        -- check for literal at the end of the response
        local m = s:match(".*%{(%d+)%}\r\n")
        if m then 
            s = self:__read(m)
            self.__current_response:add_literal(s)
        end
    elseif r_type == '+' then
        -- continuation response
        self.__current_response:add_continuation(s)
        self:__send_continuation()
    elseif r_type then
        -- tagged completion
        if not self.__tags[r_type] then
            error("Unknown tag from server: "..r_type)
        end
        self.__current_response:add_tagged(r_type, s)
    end
    return r_type
end

function IMAP4.__do_cmd(self, cmd, args, next_state)
    local function chk_next_state(r)
              if r:getTaggedResult() == 'OK' then
                  self.__state = next_state[1]
              elseif next_state[2] then
                  self.__state = next_state[2]
              end
          end

    states = ALLOWED_STATES[cmd]
    assert(states[self.__state], 
           "Command '"..cmd.."' not valid in "..self.__state.." state.")
    local rtype
    local tag = self:__next_tag()
    if self.__pipeline then
        -- Pipelined commands are built up and sent out as one transmission to
        -- the server.  So we'll need to get a tag and build the command as if
        -- we are sending it out, but instead stuff it in a buffer rather than
        -- transmit it.  In this case, return tag of added command
        cmd = self:__buildcmd(cmd, args)
        self.__tags[tag] = cmd
        -- per RFC2683 limit length to 1000 bytes.  Seems like a number pulled
        -- out of someone's nether regions, but we'll abide by it.
        if (#self.__pipelined_cmds + #tag + 1 + #cmd) > MAX_PIPELINE_SIZE then
            assert(self.__connection:send(self.__pipelined_cmds))
            self.__pipelined_cmds = ''
            local flags = 0
            while true do
                rtype = self:__response_handler(flags)
                if not rtype then
                    break
                elseif rtype:match(TAG_PAT) then
                    self:__new_response()
                    flags = NOWAIT
                end
            end
        end
        -- now add new command to buffer
        self.__pipelined_cmds = self.__pipelined_cmds..tag..' '..cmd
        -- check if we've just added a literal- if so then we need to handle it
        -- now
        if next_state or cmd:find(".*%{%d+%}\r\n$") then
            assert(self.__connection:send(self.__pipelined_cmds))
            self.__pipelined_cmds = ''
            while true do
                rtype = self:__response_handler()
                -- create a new response object on every tagged completion
                -- response
                if rtype:match(TAG_PAT) then
                    if rtype == tag then break
                    else self:__new_response()
                    end
                end
            end
            if next_state then chk_next_state(self.__current_response) end
        end
        return tag
    end
    -- synchronous processing means we sent the command and wait for the result
    self:__new_response()
    self:__send_command(tag, cmd, args)
    -- for synchronous commands, wait for the tagged completion response
    repeat
        rtype = self:__response_handler()
    until rtype == tag
    if next_state then chk_next_state(self.__current_response) end
    return self.__current_response
end

function IMAP4.__check_args(self, cmdargs, literals, ...)
    --[[
        Checks command arguments.  There are seveeral things to consider:
        1.  The imap command arguments are present
        2.  That literal substitution for named commands are accounted for
        3.  That any option commands are accoutned for
        4.  Named literals must be checked and turned into an indexed table

        Arguments:
            cmdargs:  this is a list of the arguments that go with an IMAP4Rev1
                      command.  The list should be specified in the order that
                      the IMAP4Rev1 command expects
            literals: a table, either indexed or key-value, of literal
                      substitutions for cmd arguments
            ...: A series of tables with the following entries:
                  1.  the parameter for a command argument or a blank indicating
                      literal substitution, or a space indicating an optional
                      argument that should not be used for literal substitution
                  2.  An error message explaining what argument error occured
                  3.  A flag field used when processing arguments, used for 
                      creating parenthesized lists('()'), or if a required
                      argument should be processes as a literal ('l')

        Returns:
            Nothing, but there are a number of side effects:
            1. If a key-value literal table is supplied it is converted to an 
               indexed table with values in the appropriate order
            2. The `__literals` member of the object is populated if any
               literals are specified
            3. The `__argt` table is assigned with the arguments as supplied
               by the calling method.

    --]]
    local named_argt = {...}
    local n_args = #named_argt
    local maxargs = #cmdargs
    local n_literals = 0
    local named_args = 0

    -- process the literals table- if keyed, make sure keys are valid for 
    -- this command and then put them in the proper order for the command
    if literals then 
        assert(utils.issubset(utils.keys(literals), cmdargs), 
               "Invalid literal key in literals")
        literals = utils.makeordered(literals, cmdargs)
        n_literals = #literals 
    end
    -- check if the proper number of arguments have been supplied for the 
    -- current command
    for i,v in ipairs(named_argt) do
        if v[1] ~= '' then named_args = named_args + 1 end
    end
    local arg_total = named_args + n_literals
    if maxargs ~= n_args then
        if arg_total < n_args then 
            error("Too few command args provided")
        elseif arg_total > maxargs then
            error("Too many command args provided")
        end
    elseif named_args + n_literals ~= n_args then
        utils.arg_error(named_argt, literals)
    end
    -- make object assignments prior to return
    self.__literals = literals or {}
    self.__argt = named_argt
end

function IMAP4.__build_arg_str(self)
    local arg_str = ''
    -- loop to process argument entries in __argt
    while #self.__argt ~= 0 do
        local arg = self.__argt[1][1]
        local flag = self.__argt[1][3]
        table.remove(self.__argt, 1)
        -- check if this is an optional argument that should NOT be used as a
        -- literal, do nothing if so
        if arg ~= ' ' then 
            if arg == '' then  -- check if arg is being sent as literal
                break
            else
                -- not a literal...
                -- now process argument flags- these modify the arguments to
                -- handle the idiosynchracies of certain command arguments.
                -- For instance, the APPEND command takes a literal as its 2nd
                -- argument so if it is passed in a as a normal named argument
                -- it must still go out as a literal
                arg = self:__handle_argflag(arg, flag)
                if not arg then break end
                arg_str = arg_str..' '..arg
            end
        end
    end
    if #self.__literals ~= 0 then
        arg_str = arg_str..' '..self:__add_literal()
    end
    return arg_str
end

--[[

    PUBLIC METHODS

--]]
function IMAP4.append(self, mb_name, msg_literal, opt_flags, opt_datetime)
    --[[
        Sends an IMAP4Rev1 APPEND command.

        Does NOT support the `literals` form of the command since a literal is
        used as the final component of the command
    --]]
    self:__check_args({'mb_name', 'opt_flags', 'opt_datetime', 'msg_literal'},
                      nil, 
                      { mb_name or '',
                        "You must supply a mailbox name when using 'APPEND'"},
                      { msg_literal or '',
                        "You must supply a string to append to the mailbox", 
                        'l' } )

    table.insert(self.__argt, 2, { opt_flags or ' ', '', '()'} )
    table.insert(self.__argt, 3, { opt_datetime or ' ', '' } )
    return self:__do_cmd('APPEND', self:__build_arg_str())
end

function IMAP4.authenticate(self, auth_mech, argt)
    --[[
        `auth_mech` is the text that appears after the "AUTH=" in server
        capabilities
        `argt` is a key-value table used to create a closure over the
        authenticating function
    --]]
    assert(auth.md5, [[The md5 library for lua is not installed.  Please 
                     install it in order to make use of CRAM-MD5 
                     authentication.]])
    local auth_func =  assert(auth.mechanims[auth_mech], 
                              "Authorization type not supported.")
    self.__literal_func = auth_func(argt)

    return self:__do_cmd('AUTHENTICATE', " "..auth_mech, {AUTH})
end

function IMAP4.capability(self)
    return self:__do_cmd('CAPABILITY')
end

function IMAP4.check(self)
    return self:__do_cmd('CHECK')
end

function IMAP4.close(self)
    return self:__do_cmd('CLOSE', nil, {AUTH})
end

function IMAP4.copy(self, seq_set, mb_name, literals)
    self:__check_args({ 'seq_set', 'mb_name' },
                      literals,
                      { seq_set or '',
                        "You must supply a sequence set when using 'COPY'",
                        "NQ" },
                      { mb_name or '',
                        "You must supply a mailbox name when using 'COPY'" } )
    return self:__do_cmd('COPY', self:__build_arg_str())
end

function IMAP4.create(self, mb_name, literals)
    self:__check_args({ 'mb_name' }, 
                      literals,
                      { mb_name or '', 
                        "You must supply a mailbox name when using 'CREATE'" } )
    local mb_name = self:__build_arg_str()
    return self:__do_cmd('CREATE', mb_name)
end

function IMAP4.delete(self, mb_name, literals)
    self:__check_args({'mb_name'}, 
                      literals,
                      { mb_name or '', 
                        "You must supply a mailbox name when using 'DELETE'" } )
    local mb_name = self:__build_arg_str()
    return self:__do_cmd('DELETE', mb_name)
end

function IMAP4.examine(self, mb_name, literals)
    self:__check_args({'mb_name'},
                      literals,
                      { mb_name or 'INBOX', '' } )
    local mb = self:__build_arg_str()
    return self:__do_cmd('EXAMINE', mb, {SELECT, AUTH})
end

function IMAP4.expunge(self)
    return self:__do_cmd('EXPUNGE')
end

function IMAP4.fetch(self, seq_set, data, literals)
    self:__check_args({'seq_set', 'data'},
                      literals,
                      {seq_set or '',
                       "You must provide a sequence set to fetch",
                       "NQ"},
                      {data or '',
                       "You must provide data times to fetch from messages",
                       '()' } )
    -- per RFC3501, the fetch command has 3 macros: ALL, FAST, FULL and these 
    -- must be the only value and should not be sent in parens
    MACROS = { ['ALL'] = 1, ['FAST'] = 1, ['FULL'] = 1 }
    if data and MACROS[data] then
        -- modify the 'flag' field in the table
        self.__argt[2][3] = nil
    end
    return self:__do_cmd('FETCH', self:__build_arg_str())
end

function IMAP4.list(self, reference, mb_pattern, literals)
    self:__check_args({'reference', 'mb_pattern'},
                      literals,
                      { reference or '""', '' },
                      { mb_pattern or '*', '' } )
    return self:__do_cmd('LIST', self:__build_arg_str())
end

function IMAP4.login(self, user, pw, literals)
    self:__check_args({'user', 'pw'}, literals,
                   { user or '', "You must supply a username to login with." },
                   { pw or '', "You must supply a password to login with. " })
    args = self:__build_arg_str()
    return self:__do_cmd('LOGIN', args, {AUTH})
end

function IMAP4.logout(self)
    self.__state = LOGOUT
    return self:__do_cmd('LOGOUT')
end

function IMAP4.lsub(self, reference, mb_pattern, literals)
    self:__check_args({'reference', 'mb_pattern'}, 
                      literals,
                      { reference or '""', '' },
                      { mb_pattern or '*', '' } )
    return self:__do_cmd('LSUB', self:__build_arg_str())
end

function IMAP4.noop(self)
    return self:__do_cmd('NOOP')
end

function IMAP4.rename(self, existing_name, new_name, literals)
    self:__check_args({'existing_name', 'new_name'},
                      literals,
                      { existing_name or '', 
                        "You must supply an existing mailbox name" },
                      { new_name or '',
                        "You must supply a new name for the mailbox" } )
    return self:__do_cmd('RENAME', self:__build_arg_str())
end

function IMAP4.search(self, criteria, opt_charset, literals)
    --[[
        Use of `literals` to send `opt_charset` is not supported
    --]]
    self:__check_args({'opt_charset', 'criteria'},
                      literals,
                      { criteria or '', 
                        "You must specify search criteria" } )

    table.insert(self.__argt, 1, {opt_charset or ' ', '', 'C' } )
    return self:__do_cmd('SEARCH', self:__build_arg_str())
end

function IMAP4.select(self, mb_name, literals)
    self:__check_args({'mb_name'},
                      literals,
                      { mb_name or 'INBOX', '' } )
    return self:__do_cmd('SELECT', self:__build_arg_str(), {SELECT, AUTH})
end

function IMAP4.shutdown(self)
    if self.__state ~= LOGOUT then self:logout() end
    self.__connection:close()
end

function IMAP4.starttls(self)
    if not ssl then
        error([[The imaplib `STARTTLS` command requires `ssl` from the luasec
                library- please make sure it is installed and visible to the lua
                interpreter.]])
    end
    local r = self:__do_cmd('STARTTLS')
    if r:getTaggedResult() == 'OK' then
        self.__connection:settimeout(nil)
        self.__connection = ssl.wrap(self.__connection, self.__sslparams)
        assert(self.__connection:dohandshake())
        self.__connection:settimeout(0)
    end
    return r
end

function IMAP4.status(self, mb_name, stat_item, literals)
    local stat_items = { MESSAGES = 1,
                         RECENT = 1,
                         UIDNEXT = 1,
                         UIDVALIDITY = 1,
                         UNSEEN = 1 }
    assert(stat_items[stat_item], "Invalid status item requested")
    self:__check_args({ 'mb_name', 'stat_item' }, 
                      literals,
                      { mb_name or '', 
                       "You must supply a mailbox name when using 'STATUS'" },
                      { stat_item, '', '()' } )
    return self:__do_cmd('STATUS', self:__build_arg_str())
end

function IMAP4.store(self, seq_set, msg_data_item, value, literals)
    self:__check_args({ 'seq_set', 'msg_data_item', 'value' },
                      literals,
                      { seq_set or '',
                        "You must supply a sequence set when using 'STORE'",
                        "NQ" },
                   { msg_data_item or '',
                     "You must supply a message data item when using 'STORE'" },
                     { value or '',
                       [[You must supply a value for the message data item when
                       using 'STORE']], '()'} )
    return self:__do_cmd('STORE', self:__build_arg_str())
end

function IMAP4.subscribe(self, mb_name, literals)
    self:__check_args({'mb_name'},
                      literals,
                   { mb_name or '', 
                     "You must supply a mailbox name when using 'SUBSCRIBE'" } )
    local mb_name = self:__build_arg_str()
    return self:__do_cmd('SUBSCRIBE', mb_name)
end

function IMAP4.uid(self, cmd, ...)
    local valid_uid_cmd = { COPY = 2, FETCH = 2, STORE = 3, SEARCH = 1 }
    cmd = cmd:upper()
    local n_cmd_args = valid_uid_cmd[cmd]
    if not n_cmd_args then
        error("UID command does not support "..cmd)
    end
    local argt = {...}
    if (cmd == 'SEARCH' and #argt ~= 1 and #argt ~= 2) or
       (cmd ~= 'SEARCH' and #argt ~= n_cmd_args) then
        error("Invalid arguments for UID command '"..cmd.."'")
    end

    if cmd == 'SEARCH' and #argt == 2 then
        self.__argt[1] = { argt[1], '', 'C' }
        self.__argt[2] = { argt[2], '' }
    else
        for i,v in ipairs(argt) do
            if (cmd == 'STORE' and i == 3) or
               (cmd == 'FETCH' and i == 2) then
                self.__argt[i] = { v, '', '()' }
            else
                self.__argt[i] = { v, '' }
            end
        end
    end
    table.insert(self.__argt, 1, { cmd, '' } )
    return self:__do_cmd('UID', self:__build_arg_str())
end

function IMAP4.unsubscribe(self, mb_name, literals)
    self:__check_args({'mb_name'},
                      literals,
                 { mb_name or '', 
                   "You must supply a mailbox name when using 'UNSUBSCRIBE'" } )
    local mb_name = self:__build_arg_str()
    return self:__do_cmd('UNSUBSCRIBE', mb_name)
end

function IMAP4.xatom(self, cmd, argstr)
    --[[ 
        This is to support experimental command extensions.  As such, the user
        of the function is responsible for passing in a valid argument string
        for the command.
    --]]
    -- just make sure argstr has a leading ' '
    if argstr and not argstr:find([[$ .*]]) then
        argstr = ' '..argstr
    end
    -- we'll add the command to the ALLOWED_STATES table as universal, since we
    -- don't have anything else to go on
    ALLOWED_STATES[cmd] = UNIVERSAL
    return self:__do_cmd(cmd, argstr)
end

function IMAP4.startPipeline(self)
    self.__pipeline = true
    self:__new_response() -- stage a response object when pipeline is sent
    self.__pipeline_index_start = #self.__responses
end

function IMAP4.endPipeline(self)
    assert(self.__connection:send(self.__pipelined_cmds))
    self.__pipelined_cmds = ''
    self.__pipeline = false
end

function IMAP4.readResponses(self, last_tag, first_tag)
    --[[
       When using pipelining, a batch of imap commands are sent at the same
       time in order to minimize line turnaround.  This method returns an
       iterator that allows the caller to loop through the responses to the
       pipelined commands.
       
       Arguments: last_tag:  the tag that terminates interation

                  first_tag:  the first tag to start iteration from- allows for
                              replaying old responses if so desired, or whatever
                              other use it support, a value of 0 sets it to the
                              beginning of the responses buffer
    --]]
    local response_index
    if first_tag then
        if first_tag == 0 then response_index = 0
        else
            if not self.__tags[first_tag] then
                error("Invalid tag used in readResponses")
            end
            for i, r in ipairs(self.__responses) do
                if r:getTaggedTag() == first_tag then
                    response_index = i
                    break
                end
            end
            if not response_index then
                error("Tag not found in __responses.")
            end
        end
    else
        response_index = self.__pipeline_index_start
    end

    --[[
        Iterator Function
        If tagged completion responses exist, then return them, if not then
        receive responses from the line until there is a break, then resume
        returning responses.
    --]]
    return function ()
               -- if we returned the final tag on the previous iteration,
               -- then we're done and response_index will be nil
               if not response_index then return nil end

               local flags = 0 -- if we need to read more data in from the line,
                               -- make sure we wait for new data, rather than
                               -- spinning in busywaits.
               local r = self.__responses[response_index]
               local tag = r:getTaggedTag()

               if not tag then
                   -- don't have completed response in current response
                   -- so wait for it
                   while true do
                       local rtype = self:__response_handler(flags)
                       if not rtype then
                           if #self.__responses > response_index then
                               tag = r:getTaggedTag()
                               break
                           end --if table...
                       -- check to see if we should generate a new response
                       -- object
                       elseif rtype:match(TAG_PAT) then
                           self:__new_response()
                           -- now that we've recieved a completion response, we
                           -- could return, but won't because there is likely
                           -- further data on the line.  So we'll continue
                           -- reading from the line, but as soon as there's a
                           -- break we'll stop so we can start returning
                           -- responses to the caller
                           flags = NOWAIT
                       end --if not rtype...
                   end --while...
               end

               -- nwo handle the index- assign nil if we're at the last tag
               -- it wasn't obvious how else to determine to terminate loop
               -- iteration since we don't know ahead of time how many 
               -- responses we'll get
               if tag == last_tag then
                   response_index = nil
               else
                   response_index = response_index + 1 
                   -- EDGE CASE: possible that on entry exactly 1 completed
                   -- response is waiting (literal as first entry in pipeline,
                   -- or state change command as first entry in pipeline).  In
                   -- this case, make sure that responses table is as long
                   -- as response_index- should only ever be off by 1
                   if response_index > #self.__responses then
                       self:__new_response()
                   end
               end --if not tag...
               return r
           end
end

function IMAP4.new(self, hostname, port, ssl_opts)
    -- the following 'magic' lines make this usable as an object
    local o = {}
    setmetatable(o, self)

    if not ssl_opts then ssl_opts = {} end
    -- now handle object initialization
    o.host = hostname or 'localhost'
    if not ssl_opts.protocol or ssl_opts.protocol == 'tlsv1' then
        o.port = port or IMAP4_port
    elseif ssl_opts.protocol == 'sslv23' or
           ssl_opts.protocol == 'sslv3' then
        o.port = port or IMAP4_SSL_port
    else
        error("Invalid ssl_proto value: "..ssl_opts.protocol)
    end
    o.__tagpre = 'a'
    o.__tag_num = 1
    o.__pipeline = false
    o.__pipelined_cmds = ''
    o.__pipeline_index_start = 0
    o.__responses = {}
    o.__tags = {}
    o.__literals = {}
    o.__argt = {}
    o.__received_data = ''
    o.__sslparams = {
                     mode = "client",
                     protocol = ssl_opts.protocol or 'tlsv1'
                    }
    
    -- get greeting
    o:__open_connection()
    o:__new_response()
    if not o:__response_handler() then
        error("No greeting from server "..o.host.." on port "..o.port)
    end
    if o.__current_response.__untagged['PREAUTH'] then
        o.__state = AUTH
    elseif o.__current_response.__untagged['OK'] then
        o.__state = NONAUTH
    elseif o.__current_response.__untagged['BYE'] then
        error("Connection refused by "..o.host)
    else
        for k,v in pairs(o.__welcome.__untagged) do
            error("Unrecognized greeting: "..k.." "..v[1])
        end
    end
    o.__welcome = o.__current_response

    -- prevents modification of the object once created
    o.__newindex = function(t, k, v) 
                       error("Adding new members to IMAP4 object not allowed")
                   end
    return o
end

-- export the objects
imap4 = { IMAP4 = IMAP4, 
          Response = Response }
return imap4
