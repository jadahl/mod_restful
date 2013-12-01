%%%----------------------------------------------------------------------
%%% File    : gen_restful_api.erl
%%% Author  : Jonas Ådahl <jadahl@gmail.com>
%%% Purpose : Behaviour for mod_restful API modules
%%% Created : 11 Nov 2010 by Jonas Ådahl <jadahl@gmail.com>
%%%
%%%
%%% Copyright (C) 2010   Jonas Ådahl
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(gen_restful_api).
-author('jadahl@gmail.com').

-export(
    [
        % behaviour
        behaviour_info/1,

        % utilities
        option/2,
        authenticate_admin_request/1,
        authorize_key_request/1,
        get_values/2,
        opts/2,
        host_allowed/1,
        params/2
    ]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-include("include/mod_restful.hrl").

%
% Behaviour
%

behaviour_info(callbacks) ->
    [
        {process_rest, 1}
    ].

%
% Utilities
%

-spec option(atom(), #rest_req{}) -> undefined | atom() | {atom(), term()}.
option(Key, #rest_req{options = Options} = _Request) ->
    case proplists:lookup(Key, Options) of
        {Key, true} -> Key;
        Result      -> Result
    end.

authenticate_admin_request(#rest_req{
        host = ReqHost,
        http_request = HTTPRequest}) ->
    case HTTPRequest#request.auth of
        {HTTPUser, Password} ->
            case jlib:string_to_jid(HTTPUser) of
                #jid{user = User, server = Host} = JID when (User /= []) and (Host /= []) ->
                    case ejabberd_auth:check_password(User, Host, Password) of
                        true ->
                            acl:match_rule(ReqHost, configure, JID);
                        _ ->
                            deny
                    end;
                _ ->
                    deny
            end;
        _ ->
            deny
    end.

authorize_key(Key, Opts) ->
    ConfiguredKey = opts(key, Opts),
    case {ConfiguredKey, Key} of
        {undefined, _} ->
            {error, no_key_configured};
        {Key, Key} when is_binary(Key) ->
            allow;
        _E ->
            deny
    end.

-spec authorize_key_request(#rest_req{}) -> allow | deny.
authorize_key_request(#rest_req{
        http_request = #request{
            method = 'GET',
            q = Q},
        options = Opts,
        global_options = GlobalOpts} = _Req) ->
    Key = opts(<<"key">>, Q),
    authorize_key(Key, Opts ++ GlobalOpts);
authorize_key_request(#rest_req{
        http_request = #request{method = 'POST'},
        format = Format,
        options = Opts,
        global_options = GlobalOpts} = Req) ->
    case Format of
        undefined ->
            deny;
        Format ->
            Key = get_key(Format, Req),
            authorize_key(Key, Opts ++ GlobalOpts)
    end.

get_values(#rest_req{
        data = {struct, Struct},
        http_request = #request{method = 'POST'},
        format = json}, Keys) ->
    try
        [
            case lists:keysearch(list_to_binary(atom_to_list(K)), 1, Struct) of
                {value, {_ ,V1}} when is_list(V1) -> {K, list_to_binary(V1)};
                {value, {_, V2}} ->                  {K, V2}
            end
            || K <- Keys
        ]
    catch
        error:{case_clause, _} = Error ->
            error_logger:error_msg("No value error ~p~ntrace: ~p~n",
                                   [Error, erlang:get_stacktrace()]),
            {error, missing_parameters}
    end;
get_values(#rest_req{http_request = #request{method = 'GET', q = Q}}, Keys) ->
    try
        [
            case lists:keysearch(list_to_binary(atom_to_list(K)), 1, Q) of
                {value, {_, V}} -> {K, V}
            end
            || K <- Keys
        ]
    catch
        error:{case_clause, _} = Error ->
            error_logger:error_msg("No value error ~p~ntrace: ~p~n",
                                   [Error, erlang:get_stacktrace()]),
            {error, missing_parameters}
    end;
get_values(_, _Keys) ->
    {error, bad_request}.

get_key(json, #rest_req{data = Data}) ->
    case Data of
        {struct, Struct} ->
            case lists:keysearch(<<"key">>, 1, Struct) of
                {value, {_, Key}} ->
                    Key;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

opts(Key, Opts) ->
    case lists:keysearch(Key, 1, Opts) of
        {value, {_, Value}} ->
            Value;
        _ ->
            undefined
    end.

host_allowed(Host) ->
    lists:member(Host, ejabberd_config:get_global_option(hosts, fun(V) -> V end)).

%% @spec params([atom()], #rest_req{}) -> [any()] | {error, atom()}
%%
%% @doc Takes a list of parameters, gathers them from the
%% Request record, and returns a list of their corresponding values. If not
%% all parameters were found, {error, incomplete} is returned instead. If any
%% other error occurred, params() will return {error, Reason}.

-spec params([atom()], #rest_req{}) -> [any()] | {error, atom()}.
params(Params, Request) ->
    case gen_restful_api:get_values(Request, Params) of
        L when is_list(L) ->
            case lists:unzip(L) of
                {Params, Result} ->
                    case all_non_empty(Result) of
                        true ->
                            prep_result(L);
                        _ ->
                            {error, incomplete}
                    end;
                _ ->
                    {error, incomplete}
            end;
        {error, Error} ->
            {error, Error}
    end.

all_non_empty(List) -> not lists:member([], List).

prep_result(Result) ->
    try
        lists:map(fun({Param, Value}) ->
                      NewValue = case Param of
                          username -> jlib:nodeprep(Value);
                          host     -> jlib:nameprep(Value);
                          _        -> Value
                      end,
                      if NewValue == error -> throw(prep_failed);
                         true              -> NewValue
                      end
                  end, Result)
    catch
        prep_failed ->
            error_logger:error_msg("Failed to prepare result~nresult: ~p~ntrace: ~p~n",
                                   [Result, erlang:get_stacktrace()]),
            {error, stringprep}
    end.

