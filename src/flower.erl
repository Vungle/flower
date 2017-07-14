%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Vungle
%%% @doc Flower is a data flow framework that manages filtering pipelines and
%%% result merging. This is the main flower module.
%%% @end
%%%-------------------------------------------------------------------
-module(flower).

-export([
    init/0,
    run/2, run/3,
    run_in_process/3
]).

init() ->
    bidbox_flower_flow_sup:start_link().

-spec run(map(), list()) -> {ok, list()} | error | {error, atom()}.
run(Settings, List) ->
    run(Settings, List, #{}).
-spec run(map(), list(), map()) -> {ok, list()} | error | {error, atom()}.
run(Settings, List, Context) ->
    Steps = maps:get(steps, Settings, []),
    Timeout = maps:get(timeout, Settings, 100),
    ParentPid = self(),
    {ok, FlowRef} = bidbox_flower_flow_sup:run_flow(Steps, List, Context, ParentPid),
    monitor(process, FlowRef),
    loop(Timeout, FlowRef).

%% @doc Runs the steps using the current process instead of running
%%      a child in the supervision tree. This is useful when latency
%%      is important.
%% @end
-spec run_in_process(map(), list(), map()) -> list() | error.
run_in_process(Settings, List, Context) ->
    Steps = maps:get(steps, Settings, []),
    bidbox_flower_worker:run_steps(Steps, List, Context).

loop(Timeout, FlowRef) ->
    receive
        {ok, FlowRef, Result} ->
            bidbox_flower_flow_sup:stop(FlowRef),
            {ok, Result};

        %% Wait for the flow message
        {'DOWN', _, _, _, normal} ->
            loop(Timeout, FlowRef);

        %% Error
        {'DOWN', _, _, _, Reason} ->
            {error, Reason};

        %% Unknown
        Msg ->
            lager:info("UNKNOWN MESSAGE ~p~n", [Msg]),
            loop(Timeout, FlowRef)
    after Timeout ->
        % Kill the whole process tree
        exit(FlowRef, kill),
        {error, timeout}
    end.
