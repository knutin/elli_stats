-module(elli_stats_server).
-behaviour(gen_server).

%% API
-export([start_link/2, incr/2, request/3, add_subscriber/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {subscribers = [], elli_controller}).

%%%===================================================================
%%% API
%%%===================================================================


start_link(Name, ElliController) ->
    gen_server:start_link({local, Name}, ?MODULE, [ElliController], []).

incr(Name, Key) ->
    incr(Name, Key, 1).
incr(Name, Key, Amount) ->
    gen_server:cast(Name, {incr, Key, Amount}).

request(Name, Id, Timings) ->
    gen_server:cast(Name, {request, Id, Timings}).

add_subscriber(Name, Ref) ->
    gen_server:call(Name, {add_subscriber, Ref}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ElliController]) ->
    erlang:send_after(1000, self(), push),
    {ok, #state{elli_controller = ElliController}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({request, Id, Timings}, State) ->
    UserStart = proplists:get_value(user_start, Timings),
    UserEnd   = proplists:get_value(user_end, Timings),
    record_timing(Id, timer:now_diff(UserEnd, UserStart)),
    record_timing('_total', timer:now_diff(UserEnd, UserStart)),

    {noreply, State};

handle_cast({incr, Key, Amount}, State) ->
    incr_counter(Key, Amount),
    {noreply, State}.


handle_info(push, #state{subscribers = Subscribers} = State) ->
    Stats = get_stats(State#state.elli_controller),
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

record_timing(Key, Time) ->
    case get({timing, Key}) of
        undefined ->
            put({timing, Key}, [Time]);
        Timings ->
            put({timing, Key}, [Time | Timings])
    end.


get_stats(Controller) ->
    Timings = lists:filter(fun ({{timing, _} = Key, _}) ->
                                   erase(Key),
                                   true;
                               (_) ->
                                   false
                           end, get()),

    TimeStats =
        lists:map(
          fun ({{timing, Id}, Values}) ->
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

    OpenReqs = case catch elli:get_open_reqs(Controller, 100) of
                   {ok, N} -> N;
                   {'EXIT', _} -> 0
               end,

    {[{timings, {TimeStats}}, {open_reqs, OpenReqs}]}.
