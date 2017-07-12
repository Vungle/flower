%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Vungle
%%% @doc
%%% Simple step, that echoes back whatever it receives.
%%% @end
%%%-------------------------------------------------------------------
-module(bidbox_flower_echo_step).
-behavior(bidbox_flower_step).

-export([
    run/2
]).

run(List, _Context) ->
    {ok, List}.
