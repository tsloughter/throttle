-module(throttle).

-export([init/3,
         reconfigure/3,
         check/1,
         is_throttled/1,
         throttle/1,
         erase/1]).

-define(KEY(Name), {?MODULE, Name}).

-spec init(term(), integer(), integer()) -> {ok, atomics:atomics_ref()}.
init(Name, Hwm, Window) when Hwm > 0, Window > 0 ->
    %% keeping the hwm in an atomic lets us reconfigure
    %% without changing the persistent term
    A = atomics:new(3, []),
    atomics:put(A, 2, Hwm),
    atomics:put(A, 3, Hwm - Window),
    persistent_term:put(?KEY(Name), A),
    {ok, A}.

-spec reconfigure(term(), integer(), integer()) -> ok.
reconfigure(Name, Hwm, Window) when Hwm > 0, Window > 0 ->
    A = persistent_term:get(?KEY(Name)),
    atomics:put(A, 2, Hwm),
    atomics:put(A, 3, Hwm - Window),
    ok.

-spec check(term()) -> ok | integer().
check(Name) ->
    A = persistent_term:get(?KEY(Name)),
    Hwm = atomics:get(A, 2),
    WindowMin = atomics:get(A, 3),
    case erlang:process_info(self(), message_queue_len) of
        {message_queue_len, Len} when Len > Hwm ->
            %% probably better to just do a put?
            atomics:compare_exchange(A, 1, 0, 1);
        {message_queue_len, Len} when Len < WindowMin ->
            atomics:compare_exchange(A, 1, 1, 0);
        _ ->
            ok
    end.

-spec is_throttled(term()) -> boolean().
is_throttled(Name) ->
    A = persistent_term:get(?KEY(Name)),
    atomics:get(A, 1) =:= 1.

-spec throttle(term()) -> integer().
throttle(Name) ->
    A = persistent_term:get(?KEY(Name)),
    atomics:exchange(A, 1, 1).

-spec erase(term()) -> boolean().
erase(Name) ->
    persistent_term:erase(?KEY(Name)).
