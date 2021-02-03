%%%-------------------------------------------------------------------
%% @doc svl public interface
%% @end
%%%-------------------------------------------------------------------

-module(svl_manager).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

%% apis
-export([start_link/0]).
-export([refresh/0, kill_all/0, info/0, info/1]).
-export([notify/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-type job_info() :: atom() | {atom(), integer()}.
%% The state is a map from names to a record
-record(manager_state, {pid :: pid(),
			mtime :: file:date_time(),
			job_info :: job_info()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% client side api

%% from the monitor to report status
%% the reason I use a direct message instead of a cast is I want to do custom receive
-spec notify(string(), pid(), job_info()) -> any().
notify(Name, Pid, Info) -> 
    ?MODULE ! {notify, Name, Pid, Info}.

%% from user
-spec refresh() -> any().
refresh() -> gen_server:cast(?MODULE, refresh).

%% get the infos for job names
-spec info() -> [job_info()].
info() -> gen_server:call(?MODULE, info).

%% kill all jobs and wait. This is mainly for testing
-spec kill_all() -> ok.
kill_all() -> gen_server:call(?MODULE, kill_all).

-spec info([string()]) -> [job_info()].
info(Names) -> gen_server:call(?MODULE, {info, Names}).

%% server side api

init([]) ->
    process_flag(trap_exit, true),
    {ok, refresh_state(#{})}.

terminate(_Reason, State) ->
    wait_all_jobs(stop_all_jobs(State)),
    ok.

handle_info({notify, Name, _From, halted}, State) ->
    case maps:get(Name, State, undefined) of
	undefined ->
	    ?LOG_WARNING("Unrecognized name in notify: ~ts", [Name]),
	    {noreply, State};
	#manager_state{pid = Pid, mtime = udefined} ->
	    %% if mtime is undefined then this job is supposed to be removed
	    svl_jobs_sup:delete(Pid),
	    {noreply, maps:remove(Name, State)};
	_ ->
	    %% in this case do nothing, because there must be a start on going anyway.
	    {noreply, State}
    end;
handle_info({notify, Name, From, Info}, State) ->
    case maps:get(Name, State, undefined) of
	undefined ->
	    ?LOG_WARNING("Unrecognized name in notify: ~ts", [Name]),
	    {noreply, State};
	Job_state ->
	    {noreply, maps:update(Name, Job_state#manager_state{pid = From,
								job_info = Info},
				  State)}
    end.

handle_cast(refresh, State) -> {noreply, refresh_state(State)}.

handle_call(kill_all, _From, State) ->
    {reply, ok, wait_all_jobs(stop_all_jobs(State))};
handle_call(info, _From, State) ->
    Job_infos = lists:map(fun(Each) ->
				  Job_state = maps:get(Each, State),
				  {Each, Job_state#manager_state.job_info}
			  end, maps:keys(State)),
    {reply, Job_infos, State};
handle_call({info, Names}, _From, State) ->
    Job_infos = lists:map(
		  fun(Each) ->
			  case maps:get(Each, State, undefined) of
			      undefined -> {Each, undefined};
			      #manager_state{job_info = Info} -> {Each, Info}
			  end
		  end, Names),
    {reply, Job_infos, State}.

%% internal server side functions
root_dir() ->
    case application:get_env(root_dir) of
	undefined -> lists:flatten([os:getenv("HOME", ""), "/svl"]);
	{ok, Dir} -> Dir
    end.

launch_job(Dir, Name) ->
    case svl_jobs_sup:add(Name, lists:flatten([Dir, $/, Name])) of
    	{ok, undefined} -> error("Cannot start child");
	{ok, Pid} -> #manager_state{pid = Pid, job_info = booting}
    end.

start_job(Job_state = #manager_state{pid = Pid}) ->
    gen_statem:cast(Pid, start),
    Job_state.

stop_job(Job_state = #manager_state{pid = Pid}) ->
    gen_statem:cast(Pid, stop),
    Job_state.

stop_all_jobs(State) ->
    maps:map(fun(_Name, Job_state) ->
		     stop_job(Job_state)
	     end, State).

wait_all_jobs(State) ->
    case maps:size(State) of
	0 -> State;
	_ -> wait_all_jobs(wait_any_job(State))
    end.

wait_any_job(State) ->
    receive
	{notify, Name, _From, halted} ->
	    case maps:get(Name, State, undefined) of
		undefined ->
		    ?LOG_WARNING("Unrecognized name in notify: ~ts", [Name]),
		    State;
		#manager_state{pid = Pid} ->
                    svl_jobs_sup:delete(Pid),
                    maps:remove(Name, State)
            end;
	_ ->
 	    %% ignore everything else, not important anymore
	    State
    end.

refresh_state(State) ->
    Dir = root_dir(),
    Mtime_map = load_mtime(Dir),
    patch_state(Dir, Mtime_map, State).

patch_state(Dir, Mtime_map, State) ->
    New_mtimes =
	maps:filter(
	  fun(Name, _Mtime) ->
		  not maps:is_key(Name, State)
	  end, Mtime_map),
    New_jobs =
	maps:map(
	  fun(Name, Mtime) ->
		  Job_state = launch_job(Dir, Name),
		  Job_state#manager_state{mtime = Mtime}
	  end, New_mtimes),
    Old_jobs =
	maps:map(
	  fun(Name, Job_state) ->
		  Mtime = maps:get(Name, Mtime_map, undefined),
		  Old_mtime = Job_state#manager_state.mtime,
		  New_state =
		      if Mtime == undefined -> stop_job(Job_state);
			 Mtime > Old_mtime -> start_job(stop_job(Job_state));
			 true -> Job_state
		      end,
		  New_state#manager_state{mtime = Mtime}
	  end, State),
    maps:merge(Old_jobs, New_jobs).

load_mtime(Dir) ->
    maps:from_list(lists:flatten(scan_dir("", Dir))).

scan_dir(Prefix, Dir) ->
    case file:list_dir(Dir) of
	{error, _Reason} -> [];
	{ok, Filenames} ->
	    lists:map(
	      fun(Name) ->
		      File = lists:flatten([Dir, $/, Name]),
		      New_name = Prefix ++ Name,
		      case is_legal(Name) of
			  false -> [];
			  true -> scan_file(New_name, File)
		      end
	      end, Filenames)
    end.

is_legal([$. | _Tail]) -> false;
is_legal([$# | _Tail]) -> false;
is_legal(Name) ->
    case lists:last(Name) of
	$~ -> false;
	_  -> true
    end.

scan_file(Name, File) ->
    case file:read_file_info(File) of
	{error, _Reason} -> [];
	{ok, #file_info{type = Type, mode = Mode, mtime = Mtime}} ->
	    %% rwxrwxrwx and x bit not set is false
	    if Type == directory -> scan_dir(Name ++ "/", File);
	       Type /= regular -> [];
	       Mode rem 2 == 0 -> [];
	       Mode div 8 rem 2 == 0 -> [];
	       Mode div 64 rem 2 == 0  -> [];
	       true -> {Name, Mtime}
	    end
    end.
