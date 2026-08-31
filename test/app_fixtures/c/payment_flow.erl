-module(payment_flow).
-vsn({3,0,0}).
-define(VSN, {3,0,0}).
-define(MARKER, c).
-include("../payment_flow.hrl").

sf_do_code_change(V, B) -> case {V, B} of
    {{1,0,0}, payment_flow} ->
        mark({route, {V, {3,0,0}}}),
        sf_migrate({1,0,0}, [async_m || wait,
            Reply <- async:promise_mref(make_ref()),
            Resume <- async_m:pure_return(mark({resume, payment})),
            return({payment, ?VSN, Reply, Resume})],
            #{from => payment_flow, to => payment_flow,
              steps => #{wait => #{to => wait}}});
    {{2,0,0}, payment_flow} ->
        mark({route, {V, {3,0,0}}}),
        sf_migrate({2,0,0}, [async_m || wait,
            Reply <- async:promise_mref(make_ref()),
            Resume <- async_m:pure_return(mark({resume, payment})),
            return({payment, ?VSN, Reply, Resume})],
            #{from => payment_flow, to => payment_flow,
              steps => #{wait => #{to => wait}}});
    _ -> unhandled
end.
