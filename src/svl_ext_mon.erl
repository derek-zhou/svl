%%%-------------------------------------------------------------------
%% @doc svl external program monitor
%% @end
%%%-------------------------------------------------------------------

-module(svl_ext_mon).
-behaviour(gen_statem).

-include_lib("kernel/include/logger.hrl").

%% apis
-export([start_link/1]).

%% callbacks
-export([terminate/3, code_change/4, init/1, callback_mode/0]).

%% the states
-export([halted/3, booting/3, running/3, halting/3, blackout/3]).

%% the data record to hold extended
-record(ext_mon_data, {name,
		       script,
		       buffer = <<>>,
		       crash_count = 0,
		       port}).

%% apis
start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

%% Mandatory callback functions
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.

init([Name, Script]) ->
    process_flag(trap_exit, true),
    {ok, booting,
     start(#ext_mon_data{name = Name,
			script = Script}),
     [{state_timeout, 1000, timeout}]}.

terminate(Reason, State, Data) -> 
    case State of
	halted ->
	    ?LOG_NOTICE("Orderly terminated: ~ts", [Reason]),    
	    ok;
	_ ->
	    ?LOG_ERROR("Unorderly terminated: ~ts", [Reason]),    
	    port_close(Data#ext_mon_data.port)
    end.
	    
callback_mode() -> state_functions.

%% internal functions
crash_count(#ext_mon_data{crash_count = Count}) ->
    Count.

pid(#ext_mon_data{port = Port}) ->
    case Port of
	undefined ->
	    undefined;
	_ ->
	    case erlang:port_info(Port) of
		undefined ->
		    undefined;
		Plist ->
		    proplists:get_value(os_pid, Plist)
	    end
    end.

kill(Data) ->
    case pid(Data) of
	undefined ->
	    ?LOG_ERROR("Port not opened"),
	    false;
	Pid when is_integer(Pid) ->
	    os:cmd(io_lib:format("kill ~B", [Pid])),
	    ok
    end.

blackout_period(Count) when is_integer(Count), Count >= 0, Count < 7 ->
    (1 bsl Count) * 1000;
blackout_period(_) ->
    64000.

start(Data) ->
    Port = open_port({spawn, Data#ext_mon_data.script},
		     [binary, exit_status, stderr_to_stdout]),
    ?LOG_NOTICE("Booting"),
    Data#ext_mon_data{port = Port}.

%% log the text
log(Binary, Data = #ext_mon_data{buffer = Buffer}) ->
    Lines = re:split(Binary, "\n"),
    Remain = flush_log(Lines, Buffer),
    Data#ext_mon_data{buffer = Remain}.

%% flush log until last line, return the remaining string
flush_log([Head], <<>>) ->
    Head;
flush_log([Head], Buffer) ->
    <<Buffer/binary,Head/binary>>;
flush_log([Head | Tail], Buffer) ->
    ?LOG_NOTICE([Buffer, Head]),
    flush_log(Tail, <<>>).

%% state callbacks

%% from halt
halted(cast, start, Data) ->
    svl_manager:notify(Data#ext_mon_data.name, booting),
    {next_state, booting, start(Data), [{state_timeout, 1000, timeout}]};
halted(cast, stop, Data) ->
    {keep_state, Data}.

%% from booting
%% log binary from port
booting(info, {data, Binary}, Data) ->
    {keep_state, log(Binary, Data)};
%% no error is good news
booting(state_timeout, timeout, Data) ->
    ?LOG_NOTICE("Booted"),
    svl_manager:notify(Data#ext_mon_data.name, {running, pid(Data)}),
    {next_state, running, Data#ext_mon_data{crash_count = 0}};
%% port did not happen, or quited during booting
booting(info, {'EXIT', _Port, Reason}, Data) ->
    ?LOG_ERROR("Port closed unexpectedly, reason: ~ts", [Reason]),
    Crash_count = crash_count(Data) + 1,
    svl_manager:notify(Data#ext_mon_data.name, {blackout, Crash_count}),
    {next_state, blackout, Data#ext_mon_data{port = undefined, crash_count = Crash_count},
    [{state_timeout, blackout_period(Crash_count), timeout}]};
%% got start/stop message
booting(cast, start, Data) ->
    {keep_state, Data};
booting(cast, stop, Data) ->
    kill(Data),
    svl_manager:notify(Data#ext_mon_data.name, {halting, pid(Data)}),
    {next_state, halting, Data#ext_mon_data{crash_count = 0}}.

%% from running
%% log the data
running(info, {data, Binary}, Data) ->
    {keep_state, log(Binary, Data)};
%% port closed
running(info, {'EXIT', _Port, Reason}, Data) ->
    ?LOG_ERROR("Port closed unexpectedly, reason: ~ts", [Reason]),
    svl_manager:notify(Data#ext_mon_data.name, booting),
    {next_state, booting, start(Data), [{state_timeout, 1000, timeout}]};
%% got start/stop message
running(cast, start, Data) ->
    {keep_state, Data};
running(cast, stop, Data) ->
    kill(Data),
    svl_manager:notify(Data#ext_mon_data.name, {halting, pid(Data)}),
    {next_state, halting, Data}.

%% from halting
%% log the data
halting(info, {data, Binary}, Data) ->
    {keep_state, log(Binary, Data)};
halting(info, {'EXIT', _Port, Reason}, Data) ->
    svl_manager:notify(Data#ext_mon_data.name, halted),
    ?LOG_NOTICE("Port closed, reason: ~ts", [Reason]),
    {next_state, halted, Data#ext_mon_data{port = undefined}};
%% got start/stop message
halting(cast, start, Data) ->
    %% just pretend I am running. Once the job quit it will be restarted
    svl_manager:notify(Data#ext_mon_data.name, {running, pid(Data)}),
    {next_state, running, Data};
halting(cast, stop, Data) ->
    {keep_state, Data}.

%% from blackout
blackout(state_timeout, timeout, Data) ->
    ?LOG_NOTICE("Blackout period expired"),
    svl_manager:notify(Data#ext_mon_data.name, booting),
    {next_state, booting, start(Data), [{state_timeout, 1000, timeout}]};
%% got start/stop message
blackout(cast, start, Data) ->
    svl_manager:notify(Data#ext_mon_data.name, booting),
    {next_state, booting, start(Data), [{state_timeout, 1000, timeout}]};
blackout(cast, stop, Data) ->
    svl_manager:notify(Data#ext_mon_data.name, halted),
    {next_state, halted, Data}.