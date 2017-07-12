%%%-------------------------------------------------------------------
%%% @doc This reduces the given lists by merging them together
%%% @end
%%%-------------------------------------------------------------------
-module(bidbox_flower_merge_reducer).
-behavior(bidbox_flower_reducer).
-include("../../src/flower/bidbox_flower_include.hrl").

-export([
    handle_step_done/2,
    get_result/1
]).

handle_step_done({Module, List}, #state{results = PrevResults} = State) ->
    NewState = State#state{results = maps:put(Module, List, PrevResults)},
    {ok, NewState}.

get_result(#state{results = Results}) ->
    FinalList = maps:fold(
        fun(_, List, PrevList) ->
            List ++ PrevList
        end,
        [],
        Results
    ),
    {ok, FinalList}.
