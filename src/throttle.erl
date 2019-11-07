-module(throttle).

-export([init/3,
         reconfigure/3,
         self_check/1,
         is_enabled/1,
         disable/1,
         erase/1]).

-define(KEY(Name), {?MODULE, Name}).

-record(state, {name            :: term(),
                high_water_mark :: integer(),
                window_min      :: integer()}).
-opaque state() :: #state{}.
-export_type([state/0]).

-spec init(term(), integer(), integer()) -> {ok, state()}.
init(Name, Hwm, Window) when Hwm > 0, Window > 0 ->
    persistent_term:put(?KEY(Name), true),
    {ok, #state{name=Name,
                high_water_mark=Hwm,
                window_min=Hwm - Window}}.

-spec reconfigure(integer(), integer(), state()) -> state().
reconfigure(Hwm, Window, State) when Hwm > 0, Window > 0 ->
    State#state{high_water_mark=Hwm,
                window_min=Hwm - Window}.

%% Does the message queue check and returns the new value as a boolean
-spec self_check(term()) -> boolean().
self_check(#state{name=Name,
                  high_water_mark=Hwm,
                  window_min=WindowMin}) ->
    IsEnabled = is_enabled(Name),
    case {IsEnabled, erlang:process_info(self(), message_queue_len)} of
        {true, {message_queue_len, Len}} when Len > Hwm ->
            ok = disable(Name),
            true;
        {false, {message_queue_len, Len}} when Len < WindowMin ->
            ok = enable(Name),
            false;
        {IsEnabled, _} ->
            IsEnabled
    end.

-spec is_enabled(term()) -> boolean().
is_enabled(Name) ->
    persistent_term:get(?KEY(Name)).

-spec enable(term()) -> ok.
enable(Name) ->
    persistent_term:put(?KEY(Name), true).

-spec disable(term()) -> ok.
disable(Name) ->
    persistent_term:put(?KEY(Name), false).

-spec erase(term()) -> boolean().
erase(Name) ->
    persistent_term:erase(?KEY(Name)).
