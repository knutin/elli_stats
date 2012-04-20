%% @doc Elli statistics overview
%%
%% This middleware provides a statistics overview similar to Haproxy
%% that can be accessed over HTTP.
%%
%% The user can optionally define a fun to group similar requests together
%%
%% Stats are collected in a separate process linked to the elli master process.

-module(elli_stats).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).
-export([start_demo/0]).
-compile(export_all).


handle(Req, Config) ->
    case elli_request:path(Req) of
        [<<"elli">>, <<"stats">>] ->
            file:read_file(filename:join([docroot(Config), "index.html"]));

        [<<"elli">>, <<"stats">>, <<"stream">>] ->
            ok = elli_stats_server:add_subscriber(elli_request:chunk_ref(Req)),

            {chunk, [{<<"Content-Type">>, <<"text/event-stream">>}]};

        [<<"elli">>, <<"stats">>, <<"media">> | Path] ->
            Filepath = filename:join([docroot(Config) | Path]),
            valid_path(Filepath) orelse throw({403, [], <<"Permission denied">>}),
            case file:read_file(Filepath) of
                {ok, Bin} ->
                    {ok, Bin};
                {error, enoent} ->
                    {404, <<"Not found">>}
            end;

        _ ->
            ignore
    end.



handle_event(request_complete, [Req, _ResponseCode, _ResponseHeaders,
                                _ResponseBody, Timings], Config) ->
    IdentityF = identity_fun(Config),
    elli_stats_server:request(IdentityF(Req), Timings),

    %%elli_stats_server:incr({response_code, ResponseCode}),
    ok;

handle_event(request_throw, _, _Config) ->
    elli_stats_server:incr(request_exception),
    ok;
handle_event(request_exit, _, _Config) ->
    elli_stats_server:incr(request_exit),
    ok;

handle_event(request_error, _, _Config) ->
    elli_stats_server:incr(request_error),
    ok;

handle_event(request_parse_error, [_Data], _Args) ->
    ok;

handle_event(client_closed, [_When], _Config) ->
    elli_stats_server:incr(client_closed),
    ok;

handle_event(client_timeout, [_When], _Config) ->
    elli_stats_server:incr(client_timeout),
    ok;

handle_event(elli_startup, [], _Config) ->
    case whereis(elli_stats_server) of
        undefined ->
            {ok, _Pid} = elli_stats_server:start_link(),
            ok;
        Pid when is_pid(Pid) ->
            ok
    end.


%%
%% INTERNAL HELPERS
%%


docroot(Config) ->
    proplists:get_value(docroot, Config, []).

identity_fun(Config) ->
    proplists:get_value(identity_fun, Config, fun (_Req) ->
                                                      <<"undefined">>
                                              end).

valid_path(Path) ->
    case binary:match(Path, <<"..">>) of
        {_, _} -> false;
        nomatch -> true
    end.


%%
%% DEMO
%%


start_demo() ->
    IdentityFun =
        fun (Req) ->
                case elli_request:path(Req) of
                    [<<"favicon.ico">>] -> <<"favicon">>;
                    [<<"hello">>] -> <<"hello">>;
                    _ -> <<"unknown">>
                end
        end,
    StatsConfig = [{docroot, "priv/docroot"},
                   {identity_fun, IdentityFun}],

    Config = [
              {mods, [
                      {elli_stats, StatsConfig},
                      {elli_example_callback, []}
                     ]}
             ],

    elli:start_link([{callback, elli_middleware}, {callback_args, Config}]).
