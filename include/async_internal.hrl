%% Shared callback-store encoding.
-record(callback, {
    cc,
    acc_ref,
    parent_ref
}).

-record(sf_callback, {
    frame,
    acc_ref,
    parent_ref,
    timer_ref = undefined
}).
