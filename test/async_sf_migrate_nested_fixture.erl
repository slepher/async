%%% Dynamic source for the generation-2 nested migration black box.
-module(async_sf_migrate_nested_fixture).
-export([source/4, source/5, payment_module/0]).

source(Module, Version, payment, Declarations) ->
    Header = header(Module, Version, ["run/1", "complete/2", "counter/1"]),
    Body = "run(Ref) -> mark({action, payment}), mark({step, payment}), mark({request, payment}), sf_do(payment_flow, [async_m || payment_step, Reply <- async_m:promise(Ref), Marker <- async_m:pure_return(mark({resume, payment})), return({payment, version(), Reply, Marker})]).\n",
    Common = "complete(Outcome, {Pid, Tag}) -> Pid ! {Tag, Outcome}, async_r_t:return(Outcome, async_r_t:new(identity)).\ncounter(Key) -> get(Key).\nversion() -> " ++ io_lib:format("~p", [Version]) ++ ".\nmark(Key) -> put(Key, 1 + case get(Key) of undefined -> 0; N -> N end), Key.\n",
    iolist_to_binary([Header, Body, Common, declarations(Declarations, payment,
        Version)]);
source(Module, Version, order, Declarations) ->
    source(Module, Version, order, Declarations, async_sf_migrate_nested_fixture:payment_module()).
source(Module, Version, order, Declarations, PaymentModule) ->
    Header = header(Module, Version, ["run/1", "complete/2", "counter/1"]),
    Body = io_lib:format("run({_OrderRef, PaymentRef}) -> mark({action, order}), mark({step, order}), sf_do(order_flow, [async_m || order_step, Child <- ~p:run(PaymentRef), Marker <- async_m:pure_return(mark({resume, order})), return({order, version(), Child, Marker})]).~n", [PaymentModule]),
    Common = "complete(Outcome, {Pid, Tag}) -> Pid ! {Tag, Outcome}, async_r_t:return(Outcome, async_r_t:new(identity)).\ncounter(Key) -> get(Key).\nversion() -> " ++ io_lib:format("~p", [Version]) ++ ".\nmark(Key) -> put(Key, 1 + case get(Key) of undefined -> 0; N -> N end), Key.\n",
    iolist_to_binary([Header, Body, Common, declarations(Declarations, order,
        Version)]).

payment_module() -> nested_payment_owner.

header(Module, Version, Exports) ->
    io_lib:format("-module(~p).~n-vsn(~p).~n-define(ASYNC_DO,true).~n-include_lib(\"async/include/async.hrl\").~n-export([~s]).~n", [Module, Version, string:join(Exports, ",")]).

declarations([], _Kind, _TargetVersion) -> [];
declarations(Versions, Kind, TargetVersion) ->
    ["sf_do_code_change(V, B) -> case {V,B} of ",
     [clause(Vsn, Kind, TargetVersion) || Vsn <- Versions],
     " _ -> unhandled end.\n"].

clause(Vsn, payment, TargetVersion) ->
    io_lib:format("{{~p,0,0},payment_flow} -> mark({route, {~p, ~p}}), sf_migrate({~p,0,0}, [async_m || payment_step, Step <- async_m:promise(make_ref()), Marker <- async_m:pure_return(mark({resume, payment})), return({payment, version(), Step, Marker})], #{from=>payment_flow,to=>payment_flow,steps=>#{payment_step=>#{to=>payment_step}}});", [Vsn, Vsn, TargetVersion, Vsn]);
clause(Vsn, order, TargetVersion) ->
    io_lib:format("{{~p,0,0},order_flow} -> mark({route, {~p, ~p}}), sf_migrate({~p,0,0}, [async_m || order_step, Child <- nested_payment_owner:run(make_ref()), Marker <- async_m:pure_return(mark({resume, order})), return({order, version(), Child, Marker})], #{from=>order_flow,to=>order_flow,steps=>#{order_step=>#{to=>order_step}}});", [Vsn, Vsn, TargetVersion, Vsn]).
