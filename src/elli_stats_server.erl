-module(elli_stats_server).
-behaviour(gen_server).

%% API
-export([start_link/0, incr/1, add_subscriber/1]).

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


handle_cast({incr, Key, Amount}, State) ->
    case get({counter, Key}) of
        undefined ->
            put({counter, Key}, Amount);
        Count ->
            put({counter, Key}, Count + Amount)
    end,
    {noreply, State}.


handle_info(push, #state{subscribers = Subscribers, snapshot = Snapshot} = State) ->
    Diff = counter_diff(Snapshot),
    Formatted = iolist_to_binary(["data: ", jiffy:encode({Diff}), "\n\n"]),
    erlang:send_after(1000, self(), push),
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
