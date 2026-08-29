-module(async_upgrade_badfun_fixture).

-export([cc/0]).

cc() -> fun continue/1.

continue(Value) ->
    async_r_t:return({continued, Value}, async_r_t:new(identity)).
