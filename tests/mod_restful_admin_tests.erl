%%%----------------------------------------------------------------------
%%% File    : mod_restful.erl
%%% Author  : Jonas Ådahl <jadahl@gmail.com>
%%% Purpose : Tests for mod_restful_admin
%%% Created : 28 Nov 2010 by Jonas Ådahl <jadahl@gmail.com>
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

-module(mod_restful_admin_tests).
-author('jadahl@gmail.com').

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").

-include("mod_restful.hrl").

ejabberd_commands_test() ->
    Options = [{allowed_commands, [register]}, {key, <<"secret">>}],
    Path = ["admin"],
    Tests = [
        {json,
            <<"application/json">>,
            "{\"key\":\"secret\",\"command\":\"register\",\"args\":[\"test\",\"localhost\",\"secret\"]}",
            {
                {
                    [{user, binary}, {host, binary}, {password, binary}],
                    {result, restuple}
                },
                register,
                [<<"test">>, <<"localhost">>, <<"secret">>],
                {ok, "User created"}
            },
            {ok, #rest_resp{format = json, output = [{ok, <<"User created">>}]}}
        },
        {json,
            <<"application/json">>,
            "{\"key\":\"notsecret\",\"command\":\"register\",\"args\":[\"test\",\"localhost\",\"secret\"]}",
            undefined,
            {error, not_allowed}
        },
        {json,
            <<"application/json">>,
            "{\"key\":\"secret\",\"command\":\"unregister\",\"args\":[\"test\",\"localhost\"]}",
            undefined,
            {error, not_allowed}
        }
    ],


    F = fun(Format, ContentType, Data,
            EjabberdCommand,
            Response) ->
        {ok, Format, ParsedData} = mod_restful:parse_http_request(
            #request{
                headers = [{'Content-Type', ContentType}],
                method = 'POST',
                data = Data}),

        Request = #rest_req{
            http_request = #request{method = 'POST'},
            format = Format,
            path = Path,
            data = ParsedData,
            options = Options},

        meck:new(ejabberd_commands),

        case EjabberdCommand of
            {CommandFormat, Command, Args, Result} ->
                meck:expect(ejabberd_commands, get_command_format,
                    fun(C) when C =:= Command -> CommandFormat end, [{times, 1}]),
                meck:expect(ejabberd_commands, execute_command,
                    fun(C, As) when (C =:= Command) and (As =:= Args) -> Result end, [{times, 1}]);
            undefined ->
                ok
        end,

        ?assertEqual(Response, mod_restful_admin:process_rest(Request)),

        ?assert(meck:validate(ejabberd_commands)),

        meck:unload()
    end,

    [
        F(Format, ContentType, Data, EjabberdCommand, Response)
        ||
        {Format, ContentType, Data, EjabberdCommand, Response} <- Tests
    ],

    ok.
