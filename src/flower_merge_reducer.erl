%%%-------------------------------------------------------------------
%%% @doc This reduces the given lists by merging them together
%%% @end
%%%-------------------------------------------------------------------
-module(flower_merge_reducer).
-behavior(flower_reducer).
-include("internal.hrl").

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
