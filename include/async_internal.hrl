%% Shared callback-store encoding.  The three-tuple clause in
%% async_util:decode_callback/1 is the on-upgrade compatibility boundary.
-record(callback, {
    cc,
    acc_ref,
    parent_ref = none,
    timer_ref = undefined
}).

-record(sf_callback, {
    frame,
    acc_ref,
    parent_ref = none,
    timer_ref = undefined
}).
