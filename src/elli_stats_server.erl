-module(elli_stats_server).
-behaviour(gen_server).

%% API
-export([start_link/0, incr/1, request/2, add_subscriber/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {subscribers = [], snapshot = []}).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

incr(Key) ->
    incr(Key, 1).
incr(Key, Amount) ->
    gen_server:cast(?MODULE, {incr, Key, Amount}).

request(Id, Timings) ->
    gen_server:cast(?MODULE, {request, Id, Timings}).

add_subscriber(Ref) ->
    gen_server:call(?MODULE, {add_subscriber, Ref}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:send_after(1000, self(), push),
    {ok, #state{}}.

handle_call({add_subscriber, Ref}, _From, #state{subscribers = Sub} = State) ->
    {reply, ok, State#state{subscribers = [Ref | Sub]}};

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({request, Id, Timings}, State) ->
    UserStart = proplists:get_value(user_start, Timings),
    UserEnd   = proplists:get_value(user_end, Timings),
    record_timing(Id, timer:now_diff(UserEnd, UserStart)),
    incr_counter(Id, 1),

    {noreply, State};

handle_cast({incr, Key, Amount}, State) ->
    incr_counter(Key, Amount),
    {noreply, State}.


handle_info(push, #state{subscribers = Subscribers, snapshot = Snapshot} = State) ->
    Stats = get_stats(),
    Formatted = iolist_to_binary(["data: ", jiffy:encode({Stats}), "\n\n"]),

    NewSubscribers = lists:flatmap(
                       fun (Sub) ->
                               case elli_request:send_chunk(Sub, Formatted) of
                                   ok ->
                                       [Sub];
                                   {error, closed} ->
                                       [];
                                   {error, timeout} ->
                                       []
                               end
                       end, Subscribers),

    erlang:send_after(1000, self(), push),
    {noreply, State#state{snapshot = get(), subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


counter_diff(Snapshot) ->
    Counters = lists:filter(fun ({{counter, _}, _}) -> true;
                                (_) -> false
                            end, get()),
    lists:map(fun ({{counter, Key}, Val}) ->
                      OldValue = proplists:get_value({counter, Key}, Snapshot, 0),
                      {Key, Val - OldValue}
              end, Counters).


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


get_stats() ->
    Timings = lists:filter(fun ({{timing, _} = Key, _}) ->
                                   put(Key, []),
                                   true;
                               (_) ->
                                   false
                           end, get()),

    lists:map(
      fun ({{timing, Id}, Values}) ->
              Stats = bear:get_statistics(Values),

              {Id, {[
                     {mean, proplists:get_value(arithmetic_mean, Stats)},
                     {sd, proplists:get_value(standard_deviation, Stats)},
                     {frequency, length(Values)}
                    ]}}
      end, Timings).
