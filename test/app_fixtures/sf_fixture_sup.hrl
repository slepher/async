-behaviour(supervisor).
-export([start_link/0, start_link/1, init/1]).

start_link() -> start_link(sf_fixture_sup).
start_link(Name) -> supervisor:start_link({local, Name}, ?MODULE, Name).

init(sf_fixture_sup) ->
    children([{order_sup, {?MODULE, start_link, [order_sup]}},
              {payment_sup, {?MODULE, start_link, [payment_sup]}}]);
init(order_sup) -> workers([{order_worker, {order_flow, start_link, []}, [order_flow]}]);
init(payment_sup) -> workers([{payment_worker, {payment_flow, start_link, []}, [payment_flow]}]).

children(Specs) ->
    {ok, {{one_for_one, 2, 5},
        [#{id => Id, start => Start, restart => permanent, shutdown => infinity,
           type => supervisor, modules => [?MODULE]} || {Id, Start} <- Specs]}}.
workers(Specs) ->
    {ok, {{one_for_one, 2, 5},
        [#{id => Id, start => Start, restart => permanent, shutdown => 5000,
           type => worker, modules => Modules} || {Id, Start, Modules} <- Specs]}}.
