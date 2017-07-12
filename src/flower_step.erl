%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Vungle
%%% @doc Core component of every flow pipeline. From the user perspective,
%%% everything that is not data, should be a step.
%%% @end
%%%-------------------------------------------------------------------
-module(bidbox_flower_step).

-callback run(list(), map()) ->
    {ok, list()} | {error, atom()}.
