-module(order_flow).
-vsn({3,0,0}).
-define(VSN, {3,0,0}).
-define(MARKER, c).
-include("../order_flow.hrl").

sf_do_code_change(V, B) -> case {V, B} of
    {{1,0,0}, order_flow} ->
        mark({route, {V, {3,0,0}}}),
        sf_migrate({1,0,0}, [async_m || MRefs = [make_ref()], wait,
            Replies <- async_m:par([async:promise_mref(R) || R <- MRefs]),
            Resume <- async_m:pure_return(mark({resume, order})),
            return(order_result(MRefs, Replies, Resume))],
            #{from => order_flow, to => order_flow,
              steps => #{wait => #{to => wait}}});
    {{1,0,0}, order_nested_flow} ->
        mark({route, {V, {3,0,0}}}),
        sf_migrate({1,0,0}, [async_m || MRefs = [make_ref(), make_ref()], wait,
            Replies <- async_m:par([async:promise_mref(R) || R <- MRefs]),
            Resume <- async_m:pure_return(mark({resume, order})),
            return(order_result(MRefs, Replies, Resume))],
            #{from => order_nested_flow, to => order_nested_flow,
              steps => #{wait => #{to => wait}}});
    {{1,0,0}, order_nested_converter_flow} ->
        mark({route, {V, {3,0,0}}}),
        sf_migrate({1,0,0}, [async_m || MRefs = [make_ref(), make_ref()], wait,
            Replies <- async_m:par([async:promise_mref(R) || R <- MRefs]),
            Resume <- async_m:pure_return(mark({resume, order})),
            return(order_result(MRefs, Replies, Resume))],
            #{from => order_nested_converter_flow, to => order_nested_converter_flow,
              steps => #{wait => #{to => wait,
                  convert => {?MODULE, convert_order, 2}}}});
    {{2,0,0}, order_flow} ->
        mark({route, {V, {3,0,0}}}),
        sf_migrate({2,0,0}, [async_m || MRefs = [make_ref()], wait,
            Replies <- async_m:par([async:promise_mref(R) || R <- MRefs]),
            Resume <- async_m:pure_return(mark({resume, order})),
            return(order_result(MRefs, Replies, Resume))],
            #{from => order_flow, to => order_flow,
              steps => #{wait => #{to => wait}}});
    {{2,0,0}, order_nested_flow} ->
        mark({route, {V, {3,0,0}}}),
        sf_migrate({2,0,0}, [async_m || MRefs = [make_ref(), make_ref()], wait,
            Replies <- async_m:par([async:promise_mref(R) || R <- MRefs]),
            Resume <- async_m:pure_return(mark({resume, order})),
            return(order_result(MRefs, Replies, Resume))],
            #{from => order_nested_flow, to => order_nested_flow,
              steps => #{wait => #{to => wait}}});
    {{2,0,0}, order_nested_converter_flow} ->
        mark({route, {V, {3,0,0}}}),
        sf_migrate({2,0,0}, [async_m || MRefs = [make_ref(), make_ref()], wait,
            Replies <- async_m:par([async:promise_mref(R) || R <- MRefs]),
            Resume <- async_m:pure_return(mark({resume, order})),
            return(order_result(MRefs, Replies, Resume))],
            #{from => order_nested_converter_flow, to => order_nested_converter_flow,
              steps => #{wait => #{to => wait,
                  convert => {?MODULE, convert_order, 2}}}});
    _ -> unhandled
end.
