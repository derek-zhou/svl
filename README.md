# svl
service monitoring daemon written in erlang. The basic idea is inspired by runit, but simplified.

## Basic operation

On startup, it will scan a given directory recersively following symlinks unless the file name starts with ```.``` or ```#``` or ends with ```~```, for every script that is executable for all. Then it run all the scripts in parallel with log captured. If a script exits it will be run again. If the script exits within a second the rerun will be deferred with a progressive back off algorithm. 

If a running system is shut down, then every running scripts will be killed (SIGTERM), then brutally killed in 5 seconds if any script failed to quit.

## refresh operation

If a running system receives a refresh command, it will rescan the directory, and compare the current state with the previous state:

 * If an new eligible script is found it will be ran
 * If an eligible script is removed, the script will be killed (SIGTERM)
 * If an eligible script has a newer mtime, it will be killed, and after it quits, the script will be rerun.

The script is given infinite time to quit after the kill.

## info operation

If a running system receives an info command, with 0 or more script names, it will output the states of each script, including pid. If no script is given in the command line, it will print infos for all known scripts.
