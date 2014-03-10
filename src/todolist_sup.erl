-module(todolist_sup).
-behaviour(supervisor).

%% OTP callbacks
-export([start_link/0, init/1]).

%% Internal API - not to be called directly
-export([new_task/2]).

%% OTP callback implementations
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    TaskSpec = {task_srv, 
                  {task_srv, start_link, []},
                   temporary, 2000, worker, [task_srv]},
    {ok, {{simple_one_for_one, 0, 1}, [TaskSpec]}}.
    
%% API implementations
new_task(TaskName, DueIn) ->
    supervisor:start_child(?MODULE, [TaskName, DueIn]).
