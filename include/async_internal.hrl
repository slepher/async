%% Shared callback-store encoding.  The three-tuple clause in
%% async_util:decode_callback/1 is the on-upgrade compatibility boundary.
-record(callback, {
    cc,
    acc_ref,
    sf_cc_mref = undefined
}).
