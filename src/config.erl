%% config-module.
%% functions related to the server-configuration.
%% for now, this is also the place to change the config-settings.

-module(config).
-include("bifrost.hrl").
-export([get_opt/2,get_opt/3]).


get_opt(Key, Opts, Default) ->
    case proplists:get_value(Key, Opts) of
        undefined ->
            case application:get_env(ftpserver,Key) of
                {ok, Value} -> Value;
                undefined ->  Default
            end;
        Value ->
            Value
    end.

get_opt(Key, Opts) ->
    case proplists:get_value(Key, Opts) of
        undefined ->
            case application:get_env(ftpserver,Key) of
                {ok, Value} -> Value;
                undefined ->  undefined
            end;
        Value ->
            Value
    end.
