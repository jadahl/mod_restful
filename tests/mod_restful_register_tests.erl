%%%----------------------------------------------------------------------
%%% File    : mod_restful.erl
%%% Author  : Jonas Ådahl <jadahl@gmail.com>
%%% Purpose : Tests for mod_restful_register
%%% Created : 30 Nov 2010 by Jonas Ådahl <jadahl@gmail.com>
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

-module(mod_restful_register_tests).
-author('jadahl@gmail.com').

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("mod_restful.hrl").

-define(OPTIONS, [{key, <<"secret">>}]).

-define(TRY_REGISTER(User, Host, Password, Result),
    fun(U, H, P) when
        (U =:= User) and (H =:= Host) and (P =:= Password) ->
            Result
    end).

register_test_() ->
    {foreach, fun start/0, fun stop/1,
     [
         fun register_ok/0,
         fun register_exists/0,
         fun register_error/0
     ]}.

start() ->
    meck:new(ejabberd_config),
    meck:new(ejabberd_auth),
    meck:new(ejabberd_hooks),
    meck:new(jlib),
    meck:expect(jlib, nodeprep, fun(String) -> String end),
    meck:expect(jlib, nameprep, fun(String) -> String end).

stop(_) ->
    meck:unload(jlib),
    meck:unload(ejabberd_hooks),
    meck:unload(ejabberd_auth),
    meck:unload(ejabberd_config).

register_ok() ->
    register_given_result({atomic, ok}, {simple, ok}).

register_exists() ->
    register_given_result({atomic, exists}, {error, exists}).

register_error() ->
    register_given_result({error, not_allowed}, {error, not_allowed}).

register_given_result(Result, Response) ->
    User = <<"test">>,
    Host = <<"localhost">>,
    Password = <<"foo">>,
    Data = <<"{\"key\":\"secret\",\"username\":\"test\",\"host\":\"localhost\",\"password\":\"foo\"}">>,
    Req = json_req(Data, 'POST', [<<"register">>, <<"register">>]),

    meck:expect(ejabberd_config, get_global_option,
                fun(hosts, _) -> [<<"localhost">>] end),

    meck:expect(ejabberd_auth, try_register,
        ?TRY_REGISTER(User, Host, Password, Result), [{times, 1}]),

    meck:expect(ejabberd_hooks, run_fold, 4, []),

    ?assertEqual(Response, mod_restful_register:process_rest(Req)),

    ?assert(meck:validate(ejabberd_auth)).

rest_req(Data, Format, ContentType, Method, Path) ->
    HTTPRequest = #request{
        headers = [{'Content-Type', ContentType}],
        method = Method,
        data = Data},

    {ok, Format, ParsedData} = mod_restful:parse_http_request(HTTPRequest),

    #rest_req{
        http_request = HTTPRequest,
        format = Format,
        path = Path,
        data = ParsedData,
        options = ?OPTIONS}.

json_req(Data, Method, Path) ->
    rest_req(Data, json, <<"application/json">>, Method, Path).

