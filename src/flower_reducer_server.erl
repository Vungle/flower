%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Vungle
%%% @doc
%%%   Handles reducers workflow.
%%% @end
%%%-------------------------------------------------------------------
-module(flower_reducer_server).
-behavior(gen_server).

-export([
    start_link/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("internal.hrl").

start_link(ReducerModule, RemainingFlowRefs) ->
    gen_server:start_link(bidbox_flower_reducer_server, [ReducerModule, RemainingFlowRefs], []).

init([ReducerModule, RemainingFlowRefs]) ->
    {ok, #state{module = ReducerModule, remaining_flow_refs = RemainingFlowRefs}}.

handle_call(get_results, _From, State) ->
    ReducerModule = State#state.module,
    {ok, FinalResult} = ReducerModule:get_result(State),
    {reply, FinalResult, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @doc
%%  This method is called on reducer everytime a sub-flow running in parallel
%%  has a result so that it (the reducer step) can continue its work without
%%  waiting for everyone to reply. For instance, one might want to not block waiting for
%%  a logger step to return, as the next step does not care about it.
%% @end
handle_cast({step_done, FlowRef, CallerPid, StepResult}, State) ->
    ReducerModule = State#state.module,
    case ReducerModule:handle_step_done({FlowRef, StepResult}, State) of
        {ok, ReducedState} ->
            % The reducer_server is in charge of updating the list of remaining refernces for
            % simplicity. We don't want to implement this logic in every single reducer step.
            NewFlowRefs = lists:delete(FlowRef, ReducedState#state.remaining_flow_refs),
            NewState = ReducedState#state{remaining_flow_refs = NewFlowRefs},
            case NewState#state.remaining_flow_refs of
                [] ->
                    {ok, Result} = ReducerModule:get_result(NewState),
                    CallerPid ! {break, Result};
                _ ->
                    go_on
            end,
            {noreply, NewState};
        %% reducer wants to break the flow early, so be it.
        {break, ReducedState} ->
            {ok, FinalResult} = ReducerModule:get_result(ReducedState),
            CallerPid ! {break, FinalResult},
            {noreply, ReducedState}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
