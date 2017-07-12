%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Vungle
%%% @doc
%%%  Run pipelines and spawn workers.
%%% @end
%%%-------------------------------------------------------------------
-module(bidbox_flower_worker).

-include_lib("eunit/include/eunit.hrl").

-export([
    start_link/4,
    run/5,
    run_steps/3
]).

%% @doc Starts a process that runs the flow given. The result is sent as a
%%      message to the ParentPid in the format {ok, FlowPid, ResultList}.
%% @end
-spec start_link(list(), list(), map(), pid()) -> pid().
start_link(Steps, List, Context, ParentPid) ->
    %% self() is the supervisor's pid, as the supervisor is the one that spawn this process.
    proc_lib:start_link(?MODULE, run, [Steps, List, Context, ParentPid, self()]).

run(Steps, List, Context, ParentPid, AckPid) ->
    proc_lib:init_ack(AckPid, {ok, self()}),
    Result = run_steps(Steps, List, Context),
    ParentPid ! {ok, self(), Result},
    exit(normal).

%% @doc Goes through steps list and calls each step module. If the step
%%      is a parallel step, runs each part of the flow in a separate
%%      process.
%% @end
-spec run_steps(list(), list(), map()) -> list().
run_steps([], List, _Context) -> List;
run_steps(Steps, _, _) when is_atom(Steps) -> error(bad_steps);

%% @doc Run flows in sequence.
%% @end
run_steps([Step | Rest], List, Context) when not is_tuple(Step) ->
    Result = apply(Step, run, [List, Context]),
    case Result of
        {ok, StepResult} ->
            run_steps(Rest, StepResult, Context);
        {done, StepResult} ->
            StepResult
    end;

%% @doc Run flows in parallel
%% @end
run_steps([{Steps, Reducer} | Rest], List, Context) when is_tuple(Steps) ->
    StepsList = [element(I, Steps) || I <- lists:seq(1, tuple_size(Steps))],
    ForkPid = erlang:self(),
    SpawnedFlows = [bidbox_flower_worker:start_link(to_list(Step), List, Context, ForkPid) || Step <- StepsList],
    FlowRefs = [FlowRef || {ok, FlowRef} <- SpawnedFlows],
    {ok, ReducerServerPid} = spawn_reducer(Reducer, FlowRefs),
    {_, Result} = wait_for_results(ReducerServerPid, List, Context),
    gen_server:stop(ReducerServerPid),
    run_steps(Rest, Result, Context).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

wait_for_results(ReducerServerPid, List, Context) ->
    receive
        {ok, FlowRef, Result} ->
            gen_server:cast(ReducerServerPid, {step_done, FlowRef, self(), Result}),
            wait_for_results(ReducerServerPid, List, Context);
        {break, Result} ->
            {ok, Result};
        Msg ->
            lager:info("Unexpected message arrived to Flower Worker:~p~n", [Msg]),
            wait_for_results(ReducerServerPid, List, Context)
    end.

spawn_reducer(Reducer, RemainingFlowRefs) ->
    bidbox_flower_reducer_server:start_link(Reducer, RemainingFlowRefs).

to_list(RawStep) when is_list(RawStep) -> RawStep;
to_list(RawStep) -> [RawStep].
