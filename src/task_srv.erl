-module(task_srv).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name, tref, status}).

start_link(TaskName, DueIn) ->
    gen_server:start_link(?MODULE, [TaskName, DueIn], []).

init([TaskName, DueIn]) ->
    Tref = erlang:send_after(DueIn * 1000, self(), task_due),
    {ok, #state{name = TaskName, tref = Tref, status = unfinished}}.

handle_call(complete, _From, State = #state{ tref = Tref} ) ->
    erlang:cancel_timer(Tref),
    {reply, {ok, complete}, State#state{ status = complete }};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_info(task_due, State = #state{ name = Name, status = Status }) ->
    error_logger:error_msg("Task ~s in state ~p is now due!~n", [Name, Status]),
    {noreply, State#state{status = overdue}};
handle_info(info, State = #state{ name = Name, tref = Tref, status = Status} ) ->
    case erlang:read_timer(Tref) of
        false ->
            error_logger:info_msg("Task ~s has state ~p.~n", [Name, Status]);
        Millisecs ->
            error_logger:info_msg("Task ~s has state ~p and is due in ~p secs.~n", [Name, Status, Millisecs div 1000])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% Required but unused gen_server callbacks
handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

