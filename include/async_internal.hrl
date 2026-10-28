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

%% Internal delivery status, distinct from a business Outcome tuple.
-record(async_legacy_delivery, {outcome}).
