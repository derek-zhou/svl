%%%-------------------------------------------------------------------
%% @doc svl external program monitor
%% @end
%%%-------------------------------------------------------------------

-module(svl_ext_mon).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([start_link/2]).

%% callbacks
-export([terminate/3, code_change/4, init/1, callback_mode/0]).

%% the states
-export([halted/3, booting/3, running/3, halting/3, blackout/3]).

%% the data record to hold extended
-record(ext_mon_data, {name :: string(),
		       script :: string(),
		       buffer = [] :: list(),
		       crash_count = 0 :: integer(),
		       port :: port()}).

%% apis
start_link(Name, Script) -> gen_statem:start_link(?MODULE, {Name, Script}, []).

%% Mandatory callback functions
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

init({Name, Script}) ->
    process_flag(trap_exit, true),
    logger:set_process_metadata(#{domain => [otp, svl], what => Name}),
    {ok, booting,
     start(#ext_mon_data{name = Name,
			script = Script}),
     [{state_timeout, 1000, timeout}]}.

terminate(_Reason, halted, _Data) -> ok;
terminate(_Reason, _, Data) -> kill(Data).
	    
callback_mode() -> state_functions.

%% internal functions
crash_count(#ext_mon_data{crash_count = Count}) -> Count.

pid(#ext_mon_data{port = undefined}) -> undefined;
pid(#ext_mon_data{port = Port}) ->
    case erlang:port_info(Port) of
	undefined -> undefined;
	Plist -> proplists:get_value(os_pid, Plist)
    end.

kill(Data) ->
    case pid(Data) of
	undefined -> false;
	Pid when is_integer(Pid) ->
	    os:cmd(io_lib:format("kill ~B", [Pid])),
	    ok
    end.

blackout_period(Count) when is_integer(Count), Count >= 0, Count < 7 ->
    (1 bsl Count) * 1000;
blackout_period(_) -> 64000.

start(Data = #ext_mon_data{script = Script}) ->
    Port = open_port({spawn_executable, Script},
		     [binary, stderr_to_stdout]),
    Data#ext_mon_data{port = Port}.

%% log the text
log(<<>>, Data) -> Data;
log(Binary, Data = #ext_mon_data{buffer = Buffer}) ->
    case string:take(Binary, "\n", true) of
	{_, <<>>} ->
	    Data#ext_mon_data{buffer = Buffer ++ [Binary]};
	{Leading, Trailing} ->
	    ?LOG_INFO("~ts~ts", [Buffer, Leading]),
	    [_ | Remain] = string:next_codepoint(Trailing),
	    log(Remain, Data#ext_mon_data{buffer = []})
    end.

%% state callbacks

%% from halt
halted(cast, start, Data = #ext_mon_data{name = Name}) ->
    svl_manager:notify(Name, self(), booting),
    {next_state, booting, start(Data), [{state_timeout, 1000, timeout}]};
halted(cast, stop, Data) ->
    {keep_state, Data}.

%% from booting
%% log binary from port
booting(info, {Port, {data, Binary}}, Data = #ext_mon_data{port = Port}) ->
    {keep_state, log(Binary, Data)};
%% no error is good news
booting(state_timeout, timeout, Data = #ext_mon_data{name = Name}) ->
    ?LOG_DEBUG("booted"),
    svl_manager:notify(Name, self(), {running, pid(Data)}),
    {next_state, running, Data#ext_mon_data{crash_count = 0}};
%% port did not happen, or quited during booting
booting(info, {'EXIT', Port, _Reason}, Data = #ext_mon_data{name = Name, port = Port}) ->
    ?LOG_WARNING("crashed"),
    Crash_count = crash_count(Data) + 1,
    svl_manager:notify(Name, self(), {blackout, Crash_count}),
    {next_state, blackout, Data#ext_mon_data{port = undefined, crash_count = Crash_count},
    [{state_timeout, blackout_period(Crash_count), timeout}]};
%% got start/stop message
booting(cast, start, Data) -> {keep_state, Data};
booting(cast, stop, Data = #ext_mon_data{name = Name}) ->
    kill(Data),
    svl_manager:notify(Name, self(), {halting, pid(Data)}),
    {next_state, halting, Data#ext_mon_data{crash_count = 0}}.

%% from running
%% log the data
running(info, {Port, {data, Binary}}, Data = #ext_mon_data{port = Port}) ->
    {keep_state, log(Binary, Data)};
%% port closed
running(info, {'EXIT', Port, _Reason}, Data = #ext_mon_data{name = Name, port = Port}) ->
    ?LOG_WARNING("halted unexpectedly"),
    svl_manager:notify(Name, self(), booting),
    {next_state, booting, start(Data), [{state_timeout, 1000, timeout}]};
%% got start/stop message
running(cast, start, Data) -> {keep_state, Data};
running(cast, stop, Data = #ext_mon_data{name = Name}) ->
    kill(Data),
    svl_manager:notify(Name, self(), {halting, pid(Data)}),
    {next_state, halting, Data}.

%% from halting
%% log the data
halting(info, {Port, {data, Binary}}, Data = #ext_mon_data{port = Port}) ->
    {keep_state, log(Binary, Data)};
halting(info, {'EXIT', Port, _Reason}, Data = #ext_mon_data{name = Name, port = Port}) ->
    svl_manager:notify(Name, self(), halted),
    ?LOG_DEBUG("halted"),
    {next_state, halted, Data#ext_mon_data{port = undefined}};
%% got start/stop message
halting(cast, start, Data = #ext_mon_data{name = Name}) ->
    %% just pretend I am running. Once the job quit it will be restarted
    svl_manager:notify(Name, self(), {running, pid(Data)}),
    {next_state, running, Data};
halting(cast, stop, Data) -> {keep_state, Data}.

%% from blackout
blackout(state_timeout, timeout, Data = #ext_mon_data{name = Name}) ->
    ?LOG_DEBUG("blackout period expired"),
    svl_manager:notify(Name, self(), booting),
    {next_state, booting, start(Data), [{state_timeout, 1000, timeout}]};
%% got start/stop message
blackout(cast, start, Data = #ext_mon_data{name = Name}) ->
    svl_manager:notify(Name, self(), booting),
    {next_state, booting, start(Data), [{state_timeout, 1000, timeout}]};
blackout(cast, stop, Data = #ext_mon_data{name = Name}) ->
    svl_manager:notify(Name, self(), halted),
    {next_state, halted, Data}.
