%%%----------------------------------------------------------------------
%%% File    : mod_restful.erl
%%% Author  : Jonas Ådahl <jadahl@gmail.com>
%%% Purpose : Provides a RESTful API via http
%%% Created : 19 Nov 2010 by Jonas Ådahl <jadahl@gmail.com>
%%%
%%%
%%% Copyright (C) 2010-2011   Jonas Ådahl
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

-module(mod_restful_register).
-author('jadahl@gmail.com').

-export([process/1]).

-behaviour(gen_restful_api).

-include("ejabberd.hrl").

-include("include/mod_restful.hrl").

process(Request) ->
    case gen_restful_api:authorize_key_request(Request) of
        allow ->
            process2(Request);
        _ ->
            {error, not_allowed}
    end.

process2(#rest_req{path = Path, http_request = #request{method = 'POST'}} = Request) ->
    case Path of
        [_, "register"] ->
            post_register(Request);
        [_, "unregister"] ->
            post_unregister(Request);
        [_, "change_password"] ->
            post_change_password(Request);
        _ ->
            {error, not_found}
    end;
process2(#rest_req{path = Path, http_request = #request{method = 'GET'}} = Request) ->
    case Path of
        [_, "is_registered"] ->
            get_is_registered(Request);
        _ ->
            {error, not_found}
    end.

%
% POST
%
 
post_register(Request) ->
    case username_host_password(Request) of
        [Username, Host, Password] ->
            try_register(Username, Host, Password);
        {error, Error} ->
            {error, Error}
    end.

try_register(Username, Host, Password) ->
    case ejabberd_auth:try_register(Username, Host, Password) of
        {atomic, ok} ->
            {simple, ok};
        {atomic, exists} ->
            {error, exists};
        {error, not_allowed} ->
            {error, not_allowed}
    end.

post_unregister(Request) ->
    case username_host_password(Request) of
        [Username, Host, Password] ->
            case ejabberd_auth:remove_user(Username, Host, Password) of
                ok ->
                    {simple, ok};
                E when (E == not_exists) or (E == not_allowed) or (E == bad_request) ->
                    {error, E};
                error -> 
                    {error, bad_request}
            end;
        {error, Error} ->
            {error, Error}
    end.

post_change_password(Request) ->
    case params([username, host, old_password, new_password], Request) of
        [Username, Host, OldPassword, NewPassword] ->
            case gen_restful_api:host_allowed(Host) of
                true ->
                    case ejabberd_auth:check_password(Username, Host, OldPassword) of
                        true ->
                            case ejabberd_auth:set_password(Username, Host, NewPassword) of
                                ok ->
                                    {simple, ok};
                                _ ->
                                    {error, error}
                            end;
                        _ ->
                            {error, not_allowed}
                    end;
                _ ->
                    {error, not_allowed}
            end;
        Error ->
            Error
    end.

%
% GET
%

get_is_registered(Request) ->
    case params([username, host], Request) of
        [Username, Host] ->
            case gen_restful_api:host_allowed(Host) of
                true ->
                    R = ejabberd_auth:is_user_exists(Username, Host),
                    {simple, R};
                _ ->
                    {error, not_allowed}
            end;
        {error, Error} ->
            {error, Error}
    end.

%
% Utils
%

username_host_password(Request) ->
    case params([username, host, password], Request) of
        [_, Host, _] = Result ->
            case gen_restful_api:host_allowed(Host) of
                true ->
                    Result;
                _ ->
                    {error, not_allowed}
            end;
        Error ->
            Error
    end.

params(Params, Request) ->
    case gen_restful_api:get_values(Request, Params) of
        L when is_list(L) ->
            case lists:unzip(L) of
                {Params, Result} ->
                    case all_non_empty(Result) of
                        true ->
                            prep_result(Params, Result);
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

prep_result(Params, Result) ->
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
                  end, lists:zip(Params, Result))
    catch
        prep_failed -> {error, stringprep}
    end.

