-module(todolist).
-behaviour(application).

%% Our application's API
%% These are the functions we want human users to call from the REPL
-export([new_task/2, complete_task/1]).

%% OTP application callbacks
%% These functions are automatically used by the OTP library
%% It's ok if this is a little bit confusing, but maybe the easiest way
%% to think about these functions are as event handlers which funny names
%% which are automatically called when Erlang starts your application.
-export([start/2, stop/1]).
        
%% This is a little bit of syntactic sugar so we can easily start our
%% application from the command line.  
-export([start/0]).

start() ->
    application:start(todolist).

%% OTP callbacks
start(_Type, _Arg) ->
    todolist_sup:start_link().

stop(_Arg) ->
    ok.

%% API implementation
-spec new_task/2 :: (
    TaskName :: string(),
    DueIn :: integer() ) -> {ok, TaskId :: pid()}.
% @doc This function creates a new task and sets a due-in timer (in seconds)
new_task(TaskName, DueIn) ->
    todolist_sup:new_task(TaskName, DueIn).

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
