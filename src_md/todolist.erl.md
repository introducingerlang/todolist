todolist.erl
============

This is the primary implementation of the task list application.

You'll notice that it calls functions elsewhere to help it create processes
to represent tasks.

Erlang requires a `-module` declaration for each module; this must match the
filename on disk or the compiler will complain.
```erlang
-module(todolist).
```

We also need to tell what *kind* of module we're working on. Here we
tell it that we're going to implement an `application`.  The Erlang compiler
uses this information to make sure we've implemented the API for an
application.
```erlang
-behaviour(application).
```

### API ###

We also need to tell the Erlang compiler which functions are ok for humans
or other Erlang modules to use.  We do that by making a list of the function
names and their arities.

It is a common idiom for export declarations to be repeated. One declaration
is typical for "public" functions and another is used for callbacks from the
Erlang runtime system.

These are the functions we want human users to call from the REPL.
```erlang
-export([new_task/2, complete_task/1]).
```

### OTP application callbacks ###

These functions are automatically used by the OTP library
It's ok if this is a little bit confusing, but maybe the easiest way
to think about these functions are as event handlers with funny names
which are automatically called when Erlang starts your application.
```erlang
-export([start/2, stop/1]).
```

### Some sugar ###
        
This is a little bit of syntactic sugar so we can easily start our
application from the command line.  
```erlang
-export([start/0]).
```

### Implementation ###

```erlang
start() ->
    application:start(todolist).
```

#### OTP callbacks ####
The most interesting thing to note about the function below is that
it calls into another Erlang idiom, the *supervisor*. The supervisor
is a special Erlang process which starts new processes and restarts
processes which die.  Since supervisors can start child supervisors,
it's easiest to imagine a tree of processes.  In this tree, all of the
leaf nodes are worker processes and all of the parents are supervisors.
```erlang
start(_Type, _Arg) ->
    todolist_sup:start_link().

stop(_Arg) ->
    ok.
```

#### Application API ####

These functions implement the "user visible" part of our application.
A user should make a call to `new_task` if she wants to spawn a new
task process. As part of the process state, it has a name and a timer
which warns when the task is due to be completed.

We also define a "type spec" which allows Erlang's static analysis tool
`dialyzer` to run a series of tests on the application. The docstring
below the type spec is similar to a javadoc format and is automatically
extracted by a tool called edoc. 

Notice the return value of `new_task` is a tuple `{ok, TaskId}` where
a process id (or pid) represents the task.  Returning a tagged tuple
like `{ok, Value}` or `{error, Reason}` is a very common idiom in 
Erlang libraries.

As far as the implementation goes, we forward the user supplied name
and timer to the [supervisor](todolist_sup.erl.md). The supervisor
is responsible for spawning the new task process and returning its
id to this function.
```erlang
-spec new_task/2 :: (
    TaskName :: string(),
    DueIn :: integer() ) -> {ok, TaskId :: pid()}.
% @doc This function creates a new task and sets a due-in timer (in seconds)
new_task(TaskName, DueIn) ->
    todolist_sup:new_task(TaskName, DueIn).
```

The `complete_task` function is responsible for updating the state of
a task process.  We introduce the *case* expression and the `is_process_alive` 
function which comes from the core erlang library.

If the given process id is **not** alive, we return an error to the user.

If the given process id is alive, we send a `complete` message to it
using the `call` function from the OTP gen_server library. Call is a
sychronous message which awaits a response (or a timeout). 

There is also a `cast` function in the gen_server library which 
(best-effort) delivers a message asychronously and does not wait for 
a reply.

```erlang
-spec complete_task/1 :: (
    TaskId :: pid() ) -> {ok, complete} | {error, not_found}.
% @doc This function marks a task as completed if it can be found.
complete_task(TaskId) ->
    case is_process_alive(TaskId) of
        false -> 
            {error, not_found};
        true ->
            gen_server:call(TaskId, complete)
    end.
```
