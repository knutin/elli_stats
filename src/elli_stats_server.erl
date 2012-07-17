-module(elli_stats_server).
-behaviour(gen_server).

%% API
-export([start_link/2, incr/2, request/3, add_subscriber/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Exported for testing
-export([get_stats/2]).

-record(state, {subscribers = [], elli_controller, name}).

%%%===================================================================
%%% API
%%%===================================================================


start_link(Name, ElliController) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, ElliController], []).

incr(Name, Key) ->
    incr(Name, Key, 1).
incr(Name, Key, Amount) ->
    gen_server:cast(Name, {incr, Key, Amount}).

request(Name, Id, Timings) ->
    UserStart = proplists:get_value(user_start, Timings),
    UserEnd   = proplists:get_value(user_end, Timings),
    record_timing(Name, Id, timer:now_diff(UserEnd, UserStart)),
    record_timing(Name, '_total', timer:now_diff(UserEnd, UserStart)).


add_subscriber(Name, Ref) ->
    gen_server:call(Name, {add_subscriber, Ref}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, ElliController]) ->
    erlang:send_after(1000, self(), push),
    Name = ets:new(Name, [ordered_set, named_table, public, {write_concurrency, true}]),
    {ok, #state{elli_controller = ElliController, name = Name}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({incr, Key, Amount}, State) ->
    incr_counter(Key, Amount),
    {noreply, State}.


handle_info(push, #state{subscribers = Subscribers} = State) ->
    Stats = get_stats(State#state.name, State#state.elli_controller),
    Chunk = iolist_to_binary(["data: ", jiffy:encode(Stats), "\n\n"]),
    NewSubscribers = notify_subscribers(Subscribers, Chunk),

    erlang:send_after(1000, self(), push),
    {noreply, State#state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


notify_subscribers(Subscribers, Chunk) ->
    lists:flatmap(
      fun (Sub) ->
              case elli_request:send_chunk(Sub, Chunk) of
                  ok ->
                      [Sub];
                  {error, closed} ->
                      [];
                  {error, timeout} ->
                      []
              end
      end, Subscribers).

incr_counter(Key, Amount) ->
    case get({counter, Key}) of
        undefined ->
            put({counter, Key}, Amount);
        Count ->
            put({counter, Key}, Count + Amount)
    end.

record_timing(Name, Key, Time) ->
    ets:insert(Name, {erlang:now(), Key, Time}).



iterate_ets(Now, Name) ->
    iterate_ets(Now, ets:first(Name), Name, []).

iterate_ets(_Now, '$end_of_table', _Name, Acc) -> Acc;
iterate_ets(Now, Key, Name, Acc) when Now >= Key ->
    [Value] = ets:lookup(Name, Key),
    true = ets:delete(Name, Key),
    iterate_ets(Now, ets:next(Name, Key), Name, [Value | Acc]);
iterate_ets(_Now, _Key, _Name, Acc) ->
    Acc.



get_stats(Name, _Controller) ->
    Timings = lists:foldl(
                fun ({_, Key, Time}, Acc) ->
                        Timings = case lists:keyfind(Key, 1, Acc) of
                                      {Key, Ts} -> Ts;
                                      false -> []
                                  end,
                        lists:keystore(Key, 1, Acc, {Key, [Time | Timings]})
                end, [], iterate_ets(os:timestamp(), Name)),

    TimeStats =
        lists:map(
          fun ({Id, Values}) ->
                  Stats = bear:get_statistics(Values),

                  Percentiles = proplists:get_value(percentile, Stats),
                  P95 = proplists:get_value(95, Percentiles),
                  P99 = proplists:get_value(99, Percentiles),
                  P999 = proplists:get_value(999, Percentiles),

                  {Id, {[
                         {mean, proplists:get_value(arithmetic_mean, Stats)},
                         {sd, proplists:get_value(standard_deviation, Stats)},
                         {observations, length(Values)},
                         {p95, P95},
                         {p99, P99},
                         {p999, P999}
                        ]}}
          end, Timings),

    %% OpenReqs = case catch elli:get_open_reqs(Controller, 100) of
    %%                {ok, N} -> N;
    %%                {'EXIT', _} -> 0
    %%            end,

    %%{[{timings, {TimeStats}}, {open_reqs, OpenReqs}]}.
    {[{timings, {TimeStats}}]}.
