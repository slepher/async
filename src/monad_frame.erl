%%%-------------------------------------------------------------------
%%% Frame-aware Monad sequencing capability.
%%%-------------------------------------------------------------------
-module(monad_frame).

-superclass([monad]).

-export_type([frame_mfa/0, frame_context/0]).

-type frame_mfa() :: {module(), atom(), 2}.
-type frame_context() :: #{
    protocol := 1,
    block := atom(),
    step_name := atom() | undefined,
    identity := {1, atom(), atom()} | undefined,
    module := module(),
    vsn := term(),
    env := #{atom() => term()}
}.

-callback bind_frame(
    monad:m(M, _A), frame_mfa(), frame_context(), M
) -> monad:m(M, _B) when M :: monad:class().
-callback then_frame(
    monad:m(M, _A), frame_mfa(), frame_context(), M
) -> monad:m(M, _B) when M :: monad:class().

-export([bind_frame/4, then_frame/4, default_then_frame/4]).

-spec bind_frame(monad:m(M, _A), frame_mfa(), frame_context(), M) -> monad:m(M, _B).
bind_frame(UAction, FrameMFA, Context, UMonadFrame) ->
    undetermined:map(
        fun(MonadFrame, Action) ->
            typeclass_trans:apply(
                bind_frame, [Action, FrameMFA, Context], MonadFrame, ?MODULE
            )
        end,
        UAction,
        UMonadFrame
    ).

-spec then_frame(monad:m(M, _A), frame_mfa(), frame_context(), M) -> monad:m(M, _B).
then_frame(UAction, FrameMFA, Context, UMonadFrame) ->
    undetermined:map(
        fun(MonadFrame, Action) ->
            typeclass_trans:apply(
                then_frame, [Action, FrameMFA, Context], MonadFrame, ?MODULE
            )
        end,
        UAction,
        UMonadFrame
    ).

-spec default_then_frame(monad:m(M, _A), frame_mfa(), frame_context(), M) -> monad:m(M, _B).
default_then_frame(Action, IgnoreInputMFA, Context, MonadFrame) ->
    bind_frame(Action, IgnoreInputMFA, Context, MonadFrame).
