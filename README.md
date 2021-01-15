# svl
Service monitoring daemon written in erlang. The basic idea is inspired by [runit](http://smarden.org/runit/), but simplified.

## Basic operation

On startup, it will scan a given directory recursively and follow symlinks unless the file name starts with ```.``` or ```#``` or ends with ```~```, to figure out all scripts that are executable. Then it will run all the scripts in parallel with log captured. If a script exits it will be rerun. If the script exits within a second then the rerun will be deferred with a progressive back off algorithm. 

If a running system is shut down, then every running scripts will be killed (SIGTERM), the tool itself wait upto 5 seconds until everything quits.

If a running system receives a refresh command, it will rescan the directory, and compare the current state with the previous state:

 * If an new eligible script is found it will be ran
 * If an eligible script is removed, the script will be killed (SIGTERM)
 * If an eligible script has a newer mtime, it will be killed, and after it quits, the script will be reran. The script is given infinite time to quit after the kill.

If a running system receives an info command, with 0 or more script names, it will output the states of each script, including pid. If no script is given in the command line, it will print infos for all known scripts.

## Configuration

The root dir of all the scripts is configured via `root_dir` key in the aplication `svl_app`. If absent it defaults to `$HOME/svl`. You may also want to configure the logger of erlang. The application captures all outputs from the jobs and log them via the log facility `notice`. No other log message is sent through `notice`, but some are sent via `error` or `warning`.

The script can be any thing that is executable. If you are using a shell script, you most likely want to have an `exec` at the very end, so the script become the daemon. You daemon should not daemonize itself, and should continue to keep stdout/stderr opened for logging. 

``` shell
#!/bin/sh
exec my_daemon
```

Unlike with runit, you do not need to have a `exec 2>&1` line. The tool redirect stderr to stdout from the erlang side already.

## API

`svl_manager:refresh/0`: refresh the system, start/stop/restart jobs as necessary. If you want to temoprarily stop one script without removing it, you can turn off the executable bit. If you want to force a restart of a job, the easiest way is to `touch` the script.

`svl_manager:info/0`: return a list of tuples, for each of the recognized script. Each is {Name, Info} where Name is the Name of the script and Info can be:

* `booting`: This script is starting up. The tool assumes that if a script runs for more than a second then it is stable and considered `running`.
* {`running`, `Pid`}: The script is running, and Pid is the OS pid of the script.
* {`halting`, `Pid`}: The script is running with stop already issued. Pid is the OS pid.
* {`blackout`, `Crash_count`}: The script is not running because of the progressive back off. It has crashed `Crash_count` times.

`svl_manager:info/1`: Given a list of script names, return the list of {Name, Info} corresponding to the input list. If a given name is not found in the system the Info will be the atom `undefined`.
