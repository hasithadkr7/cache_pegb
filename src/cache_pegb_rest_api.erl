-module(cache_pegb_rest_api).

-export([init/2, terminate/3]).

init(#{method := Method} = Request, State) ->
    Response = handle_req(Method, Request),
    {ok, Response, State}.

handle_req(<<"GET">>, Request) ->
    io:format("handle_req|GET|Request:~p ~n", [Request]),
    case format_query_params(Request) of
        {Key} ->
            case get_cache(Key) of
                {ok, Value} ->
                    send_response(200, Request, Key, Value);
                {error, not_found} ->
                    send_response(404, Request)
            end;
        {error, invalid} ->
            send_response(400, Request)
    end;

handle_req(<<"POST">>, Request) ->
    io:format("handle_req|POST|Request:~p ~n", [Request]),
    case format_query_params(Request) of
        {error, invalid} ->
            send_response(400, Request, "error", "Bad Request");
        {Key, Value} ->
            case insert_or_update_cache(Key, Value) of
                true ->
                    send_response(204, Request);
                _ ->
                    send_response(500, Request, "error", "Internal Error")
            end;
        _ ->
            send_response(400, Request, "error", "Bad Request")
    end;

handle_req(<<"DELETE">>, Request) ->
    io:format("handle_req|DELETE|Request:~p ~n", [Request]),
    case format_query_params(Request) of
        {Key} ->
            case delete_cache(Key) of
                {ok, success}->
                    send_response(204, Request);
                {error, error} ->
                    send_response(500, Request, "error", "Internal Error")
            end;
        {error, invalid} ->
            send_response(400, Request, "error", "Bad Request")
    end;

handle_req(_Method, Request) ->
    io:format("handle_req|Unhandled|Request:~p ~n", [Request]).

terminate(_Reason, _Req, _State) ->
    ok.

format_query_params(Request) ->
    io:format("format_query_params|Qs:~p ~n", [cowboy_req:parse_qs(Request)]),
    case cowboy_req:parse_qs(Request) of
        [{<<"key">>, _Key},{<<"value">>, <<>>}] ->
            {error, invalid};
        [{<<"key">>, <<>>},{<<"value">>, _Data}] ->
            {error, invalid};
        [{<<"key">>, Key},{<<"value">>, <<"true">>}] ->
            {format_param_value(Key), true};
        [{<<"key">>, Key},{<<"value">>, <<"false">>}] ->
            {format_param_value(Key), false};
        [{<<"key">>, _Key},{<<"value">>,true}] ->
            {error, invalid};
        [{<<"key">>, Key},{<<"value">>, Data}] ->
            {format_param_value(Key), format_param_value(Data)};
        [{<<"key">>, <<>>}] ->
            {error, invalid};
        [{<<"key">>, Key}] ->
            {format_param_value(Key)};
        _ ->
            {error, invalid}
    end.

format_param_value(BinVal)->
    StrVal = binary_to_list(BinVal),
    case catch list_to_integer(StrVal) of
        {'EXIT', _} ->
            case catch list_to_float(StrVal) of
                {'EXIT', _} ->
                    StrVal;
                FloatVal ->
                    FloatVal
            end;
        IntVal ->
            IntVal
    end.

insert_or_update_cache(Key, Value) ->
    ets:insert(pegb_cache, {Key, Value}).
    
get_cache(Key) ->
    case catch ets:lookup(pegb_cache, Key) of
        [] ->
            io:format("get_cache|error|Key:~p ~n", [Key]),
            {error, not_found};
        [{Key, Value}] ->
            {ok, Value}
    end.
    
delete_cache(Key) ->
    ets:delete(pegb_cache, Key).

	
create_json_obj(Key, Value) when is_list(Value)->
    lists:concat(["{\"",Key,"\":\"",Value,"\"}"]);
create_json_obj(Key, Value) when is_boolean(Value)->
    lists:concat(["{\"",Key,"\":",Value,"}"]);    
create_json_obj(Key, Value) when is_integer(Value)->
    lists:concat(["{\"",Key,"\":",Value,"}"]).
    
send_response(StatusCode, Request) ->
    cowboy_req:reply(StatusCode, Request).
    
send_response(StatusCode, Request, Key, Value) ->
    cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, create_json_obj(Key, Value), Request).

