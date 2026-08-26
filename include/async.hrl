-include_lib("astranaut/include/macro.hrl").
-ifndef(ASYNC_DO).
-define(ASYNC_DO, true).
-use_macro({async_ext, async_do/1, [{alias, async_do}, formatter]}).
-endif.

-ifndef(ASYNC_SF_DO).
-define(ASYNC_SF_DO, true).
-compile({parse_transform, async_sf_transform}).
-endif.
