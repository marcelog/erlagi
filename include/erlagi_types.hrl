-record(agicall, {
    environment = [],
    read = fun() -> erlang:error("Not implemented") end,
    send = fun(_) -> erlang:error("Not implemented") end,
    close = fun() -> erlang:error("Not implemented") end
}).

-record(agiresult, {
    result = false,
    timeout = true,
    digits = false,
    offset = false,
    data = false,
    cmd = false,
    raw = false
}).

-record(agilog, {
    filename = false,
    iodevice = false
}).


