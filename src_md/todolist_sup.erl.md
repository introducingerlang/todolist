todolist_sup.erl
================

This file implements an OTP supervisor.  A supervisor's job in an Erlang
application is to start workers and then re-start them if one or more
of them crash.

The Erlang idiom is to **let it crash** and supervisors are the reason
why this fault tolerance strategy works.

```erlang
-module(todolist_sup).
-behaviour(supervisor).
```

We need to tell the Erlang compiler what functions are ok for other
modules to call. That is done by using the `-export` module attribute.

```erlang
%% OTP callbacks
-export([start_link/0, init/1]).
```

Erlang doesn't have a notion of a "private" or "protected" API. A
function is either public or not.  So a lot of time it's good manners
rather than an explicit enforcement mechanism in the runtime that 
dictates when and how one ought to use a function.
```erlang
%% Internal API - not to be called directly
-export([new_task/2]).
```

The function below is called by [todolist:start/2](todolist.erl.md) 

The `?MODULE` construction is one of a few built in file macros
the Erlang compiler gratuitously supplies. In this case, it is replaced
by the value of the `-module` attribute. So we are telling the Erlang
runtime system that we want to spawn a supervisor, name it the same
name as a module and that we have no arguments to pass to `init`.
```erlang
%% OTP callback implementations
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
```

The `init` function is called by OTP in response to the `start_link`.
The purpose of this function is to specify a restart strategy - 
`simple_one_for_one` in this case, and then some restart timers and 
thresholds after which the supervisor should give up restarting its
children.

The `TaskSpec` variable below tells the supervisor a ton of 
information about *how* to create a new task process.

For example, it tells the supervisor what module, function and 
arguments to use to create a new task process. It also makes clear
that a task is a `worker` type process.
```erlang
init([]) ->
    TaskSpec = {task_srv, 
                  {task_srv, start_link, []},
                   temporary, 2000, worker, [task_srv]},
    {ok, {{simple_one_for_one, 0, 1}, [TaskSpec]}}.
```

Here we are using the `start_child` function from the OTP supervisor
library to create a new task process. Since we are using a 
`simple_one_for_one` supervisor, all we have to do is pass the
arguments in a list and they will be picked up by the 
[task_srv](task_srv.erl.md) module's `start_link` function.

```erlang
%% API implementations
new_task(TaskName, DueIn) ->
    supervisor:start_child(?MODULE, [TaskName, DueIn]).
```
