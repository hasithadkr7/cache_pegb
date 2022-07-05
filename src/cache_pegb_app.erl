%%%-------------------------------------------------------------------
%% @doc cache_pegb public API
%% @end
%%%-------------------------------------------------------------------

-module(cache_pegb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        { <<"localhost">>, [{<<"/cache">>, cache_pegb_rest_api, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(cache_pegb_rest_api,
        [{port, 8000}],
        #{env => #{dispatch => Dispatch}}
    ),
    ets:new(pegb_cache, [set, public, named_table, {keypos,1}]),
    cache_pegb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
