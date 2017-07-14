%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Vungle
%%% @doc
%%%    A component that comes after a parallel flow must implement this
%%% behaviour in order to "reduce" a list of lists into one
%%%
%%% Reducer components must implement:
%%%
%%% <pre>-callback handle_step_done({list(), atom()}, state()) ->
%%%     {ok, state()} | {break, state()}.</pre>
%%% Which is called by flower everytime a paralell work is finished
%%%
%%% <pre>-callback get_result(state()) ->
%%%     {ok, list()} | {error, atom()}.</pre>
%%% Which should return the reduced state.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(flower_reducer).
-include("internal.hrl").

%% Called everytime the worker gets a reducer. Reducer should return
%% {ok, NewState} unless it wants to continue before getting all results.
%% In this case, it should return {break, NewState}.
%%
%% This method should *NOT* return a list of results. There is a method for this purpose.
%% @end
-callback handle_step_done({list(), atom()}, state()) ->
    {ok, state()} | {break, state()}.

%% Return the reduced results given a state with all results from previous steps running in parallel.
-callback get_result(state()) ->
    {ok, list()} | {error, atom()}.
