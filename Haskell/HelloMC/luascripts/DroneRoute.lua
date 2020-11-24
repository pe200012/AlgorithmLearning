
local inspect = require "inspect"
local json = require "json"
local internet = require "internet"
local com = require "component"
local geolyzer = com.geolyzer
-- local thread = require "thread"
local dep = {}
local RPCBuilder = dofile("rpc.lua")
local drone = require "drone"
local sides = require "sides"
local uuid = require "uuid"
dep.serialize = json
dep.uuid = uuid
dep.transport = {}

local colorHex = {
    white = 0x00,
    orange = 0x01,
    magenta = 0x02,
    lightBlue = 0x03,
    yellow = 0x04,
    lime = 0x05,
    pink = 0x06,
    gray = 0x07,
    lightGray = 0x08,
    cyan = 0x09,
    purple = 0x0A,
    blue = 0x0B,
    brown = 0x0C,
    green = 0x0D,
    red = 0x0E,
    black = 0x0F
}

function dep.transport.tcp()
    return {}
end

function dep.transport.connect(sock, address, port)
    sock.internal = internet.open(address, port)
    return true
end

function dep.transport.send(sock, data)
    return sock.internal:write(data)
end

function dep.transport.recv(sock)
    return sock.internal:read()
end

function dep.transport.close (sock)
    return sock.interal:close()
end

dep.thread = {}
--[[ 
function dep.thread.create (f)
    return function (...)
        return thread.create(f, ...)
    end
end
 ]]
local RPC = RPCBuilder:inject(dep)

local function hasNameParameter (t)
    for k,_ in pairs(t) do
        if type(k) == "string" then
            return true
        end
    end
    return false
end

local ServerCommand = {
    command = "ServerCommand"
}

function ServerCommand:new(method, params)
    local instance = self
    instance.method = method
    instance.params = params
    return instance
end

function ServerCommand:fromResponse (res)
    local command = self
    command.method = res.method
    command.params = res.params
    return command
end

function ServerCommand:execute(_env)
    local env = _env or _G
    local oldG = _G
    _G = env
    local r = nil
    if type(self.params) == "nil" then
        r = env[self.method]()
    elseif type(self.params) ~= "table" or hasNameParameter(self.params) then
        r = env[self.method](self.params)
    else
        r = env[self.method](table.unpack(self.params))
    end
    _G = oldG
    return r
end

local function readBit(n)
    local bits = 0
    bits = bits * 16 + colorHex[geolyzer.analyze(sides.down).properties.color]
    for i = 2, n, 1 do
        drone.move(1,0,0)
        bits = bits * 16 + colorHex[geolyzer.analyze(sides.down).properties.color]
    end
    return bits
end

local function wrapper(f, ...)
    local args = table.pack(...)
    return function ()
        return f(table.unpack(args))
    end
end

local function ignore (...)
    return 1
end

local env = {
    forward = wrapper(drone.move, 1, 0, 0),
    backward = wrapper(drone.move, -1, 0, 0),
    up = wrapper(drone.move, 0, 1, 0),
    down = wrapper(drone.move, 0, -1, 0),
    turnLeft = ignore,
    turnRight = ignore,
    turnAround = ignore,
    stop = os.exit,
    readBit = readBit,
    sides = sides
}

local function main ()
    local client = RPC.Client:new()
    client:connect("localhost", 10021)
    local err, port = client:call("walkingLoop")
    if not err then
        error("Unsuccessful connection to server!")
    end
    print("got portnumber: "..port)
    local sock = internet.open("localhost", port)
    local command = nil
    while true do
        command = ServerCommand:fromResponse(json.decode(sock:read()))
        print(inspect(command))
        local result = command:execute(env)
        print(inspect(result))
        sock:write(json.encode(result).."\n")
    end
end

main()
