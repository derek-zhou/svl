%%%-------------------------------------------------------------------
%% @doc svl jobs supervisor. It supervise all danimic jobs in svl
%% @end
%%%-------------------------------------------------------------------


-module(svl_jobs_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([add/2, delete/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => svl_ext_mon,
                    start => {svl_ext_mon, start_link, []},
                    shutdown => 1000}],
    {ok, {SupFlags, ChildSpecs}}.

add(Name, Job_info) ->
    supervisor:start_child(?MODULE, [Name, Job_info]).

delete(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).
