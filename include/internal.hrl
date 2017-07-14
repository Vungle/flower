%%% @copyright (C) 2016, Vungle
%%% @doc Flower internal models.
%%% @end

-record(state, {module                   :: module(),
                results = #{}            :: map(),
                remaining_flow_refs = [] :: list()
}).

-type state() :: #state{}.

-export_type([
    state/0
]).
