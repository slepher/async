-module(async_sf_cc_linked_fixture).
-export([async_callback/2, direct_callback/2, state_callback/3,
    crash_callback/3, nested_undef_callback/3]).

-record(state, {callbacks = #{}, events = []}).

async_callback(Outcome, {Pid, Token}) ->
    Pid ! {linked_callback, Token, Outcome},
    async_r_t:return(Outcome, async_r_t:new(identity)).

direct_callback(Outcome, {Pid, Token}) ->
    Pid ! {linked_callback, Token, Outcome},
    ignored_return.

state_callback(Outcome, {Pid, Token}, #state{} = State) ->
    Pid ! {linked_callback, Token, Outcome, State#state.callbacks},
    State#state{events = [{Token, Outcome} | State#state.events]}.

crash_callback(Outcome, {Pid, Token}, #state{} = State) ->
    Pid ! {linked_callback, Token, Outcome, State#state.callbacks},
    erlang:error(linked_callback_crash).

nested_undef_callback(Outcome, {Pid, Token}, #state{} = State) ->
    Pid ! {linked_callback, Token, Outcome, State#state.callbacks},
    async_sf_cc_linked_missing:missing().
