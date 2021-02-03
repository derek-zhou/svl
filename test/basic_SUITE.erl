-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_info/1, test_refresh/1, test_kill_all/1]).

all() -> [test_info, test_refresh, test_kill_all].

init_per_suite(Config) ->
    Dir = ?config(priv_dir, Config),
    application:set_env(svl, root_dir, Dir),
    add_scripts(Dir),
    {ok, _} = application:ensure_all_started(svl),
    Config.

end_per_suite(Config) ->
    Dir = ?config(priv_dir, Config),
    remove_scripts(Dir),
    ok.

test_info(_Config) ->
    Infos = svl_manager:info(),
    3 = length(Infos),
    [{"true", _}, {"false", undefined}] = svl_manager:info(["true", "false"]).

test_refresh(_Config) ->
    svl_manager:refresh(),
    3 = length(svl_manager:info()).

test_kill_all(_Config) ->
    ok = svl_manager:kill_all(),
    [] = svl_manager:info().

add_scripts(Dir) ->
    True_script =
	<<"#!/bin/sh\n"
	  "echo \"Hi there\"\n"
	  "exec true\n">>,
    Sleep10_script =
	<<"#!/bin/sh\n"
	  "echo \"Hi there, I sleep 10\"\n"
	  "exec sleep 10\n">>,
    Sleep20_script =
	<<"#!/bin/sh\n"
	  "echo \"Hi there, I sleep 20\"\n"
	  "exec sleep 20\n">>,
    Sleep_dir = lists:flatten(Dir, "sleep/"),
    add_script(True_script, "true", Dir),
    file:make_dir(Sleep_dir),
    add_script(Sleep10_script, "sleep10", Sleep_dir),
    add_script(Sleep20_script, "sleep20", Sleep_dir),
    ok.

remove_scripts(Dir) ->
    Sleep_dir = lists:flatten(Dir, "sleep/"),
    remove_script("sleep10", Sleep_dir),
    remove_script("sleep20", Sleep_dir),
    file:del_dir(Sleep_dir),
    remove_script("true", Dir).

remove_script(Name, Dir) ->
    File = lists:flatten([Dir, Name]),
    file:delete(File).

add_script(Text, Name, Dir) ->
    File = lists:flatten([Dir, Name]),
    file:write_file(File, Text),
    file:change_mode(File, 8#755).

    

