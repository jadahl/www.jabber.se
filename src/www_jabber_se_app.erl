-module (www_jabber_se_app).
-export ([start/2, stop/0, stop/1, out/1, out/2, out/3]).
-behavior(application).

-include("include/config.hrl").

-define(PORT, 8000).

start(_, _) ->
    % start jabber.se modules
    lists:foreach(fun(Module) -> {Module, start}() end, ?MODULES),

    % start yaws
    yaws_bundle:start().

stop(_) -> stop().
stop() ->
    yaws_bundle:stop().

out(Arg) ->
    RequestBridge = simple_bridge:make_request(yaws_request_bridge, Arg),
    ResponseBridge = simple_bridge:make_response(yaws_response_bridge, Arg),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    nitrogen:run().

out(Arg, Module) -> out(Arg, Module, "").

out(Arg, Module, PathInfo) ->
    io:format("WARNING! Unhandled ~p ~p ~p ~n", [Arg, Module, PathInfo]).

