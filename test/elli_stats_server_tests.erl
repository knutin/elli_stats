-module(elli_stats_server_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EXAMPLE_REQUEST, [{user_start,{1342,184804,683673}},
                          {user_end,{1342,184804,683682}},
                          {headers_end,{1342,184804,683643}},
                          {request_start,{1342,184804,683630}},
                          {body_end,{1342,184804,683647}},
                          {accepted,{1342,184804,630275}},
                          {request_end,{1342,184804,683709}}]).



ets_test() ->
    {ok, _} = elli_stats_server:start_link(foo, self()),

    elli_stats_server:request(foo, <<"bar">>, ?EXAMPLE_REQUEST),
    elli_stats_server:request(foo, <<"bar">>, ?EXAMPLE_REQUEST),
    elli_stats_server:request(foo, <<"bar">>, ?EXAMPLE_REQUEST),
    elli_stats_server:request(foo, <<"bar">>, ?EXAMPLE_REQUEST),
    elli_stats_server:request(foo, <<"bar">>, ?EXAMPLE_REQUEST),
    elli_stats_server:request(foo, <<"bar">>, ?EXAMPLE_REQUEST),

    Stats = elli_stats_server:get_stats(foo, self()),
    ?assertEqual({[{timings,{[{'_total',{[{mean,9.0},
                                          {sd,0.0},
                                          {observations,6},
                                          {p95,9},
                                          {p99,9},
                                          {p999,9}]}},
                              {<<"bar">>,
                               {[{mean,9.0},
                                 {sd,0.0},
                                 {observations,6},
                       {p95,9},
                                 {p99,9},
                                 {p999,9}]}}]}}]}, Stats).
