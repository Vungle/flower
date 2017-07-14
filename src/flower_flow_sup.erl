%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Vungle
%%% @doc Flower workers' supervisor. Gotta keep an eye on these guys...
%%% @end
%%%-------------------------------------------------------------------
-module(flower_flow_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    run_flow/4,
    stop/1
]).

%% Supervisor callbacks
-export([
    init/1
]).

-define(MAX_RESTARTS, 0).
-define(RESTARTS_IN_SECONDS, 1).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, bidbox_flower_flow_sup}, bidbox_flower_flow_sup, []).

stop(ChildId) ->
    supervisor:terminate_child(bidbox_flower_flow_sup, ChildId).

run_flow(Steps, List, Context, ParentPid) ->
    Worker = {make_ref(),
              {bidbox_flower_worker, start_link, [Steps, List, Context, ParentPid]},
              temporary,
              brutal_kill,
              worker,
              [bidbox_flower_worker]},
    supervisor:start_child(bidbox_flower_flow_sup, Worker).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    RestartStrategy = {one_for_one,
                       ?MAX_RESTARTS,
                       ?RESTARTS_IN_SECONDS},
    {ok, {RestartStrategy, []}}.
