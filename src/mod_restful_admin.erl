%%%----------------------------------------------------------------------
%%% File    : mod_restful_admin.erl
%%% Author  : Jonas Ådahl <jadahl@gmail.com>
%%% Purpose : Provides admin interface via mod_restful
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
%%%
%%% API
%%%
%%% To run a command, using admin@localhost:secret as basic authentication:
%%%   POST /api/admin HTTP/1.1
%%%   Host: example.net
%%%   Authorization: Basic YWRtaW5AbG9jYWxob3N0OnNlY3JldAo==
%%%   Content-Type: application/json
%%%   Content-Length: 63
%%%
%%%   {"command":"register","args":["test","example.net","secret"]}
%%%
%%% To run a command, using a shared secret key:
%%%   POST /api/admin HTTP/1.1
%%%   Host: example.net
%%%   Content-Type: application/json
%%%   Content-Length: 78
%%%
%%%   {"key":"secret","command":"register","args":["test","example.net","secret"]}
%%%
%%% Using a shared secret key, one need to configure mod_restful_admin as follows:
%%%   {mod_restful, [
%%%                  {api, [
%%%                         {["admin"], mod_restful_admin, [{key, "secret"}, {allowed_commands, [register]}]}
%%%                        ]}
%%%                 ]}
%%%
%%% Leaving out {key, "secret"} an admin credentials has to be specified using
%%% HTTP Basic Auth.
%%%


-module(mod_restful_admin).
-author('jadahl@gmail.com').

-export([process/1]).

-behaviour(gen_restful_api).

-include("ejabberd.hrl").

-include("include/mod_restful.hrl").

process(#rest_req{http_request = #request{method = Method}, path = Path} = Req) ->
    case Method of
        'POST' ->
            case tl(Path) of
                [] ->
                    case authorized(Req) of
                        allow ->
                            do_process(Req);
                        deny ->
                            {error, not_allowed}
                    end;
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.

do_process(Request) ->
    case parse_request(Request) of
        {command, Command, Args} ->
            case command_allowed(Command, Request) of
                allow ->
                    case run_command(Command, Args, Request) of
                        {error, Reason} ->
                            {error, Reason};
                        Result ->
                            Result
                    end;
                deny ->
                    {error, not_allowed}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_request(#rest_req{format = json, data = Data}) ->
    parse_json(Data).

parse_json({struct, Struct}) ->
    case lists:foldl(fun(El, In) ->
                         case In of
                             {Command, Args} ->
                                 case El of
                                     {<<"command">>, NewCommand} ->
                                         {NewCommand, Args};
                                     {<<"args">>, NewArgs} ->
                                         {Command, NewArgs};
                                     _ ->
                                         {Command, Args}
                                 end;
                             _ ->
                                 undefined
                         end
                     end, {undefined, undefined}, Struct) of
        {CMD, ARGV} when is_binary(CMD) and is_list(ARGV) ->
            {command,
                list_to_atom(binary_to_list(CMD)),
                [binary_to_list(ARG) || ARG <- ARGV]};
        _ ->
            {error, bad_request}
    end;
parse_json(_) ->
    {error, bad_request}.

command_allowed(Command, #rest_req{options = Options}) ->
    case gen_restful_api:opts(allowed_commands, Options) of
        undefined ->
            deny;
        AllowedCommands ->
            case lists:member(Command, AllowedCommands) of
                true -> allow;
                _ -> deny
            end
    end.

authorized(Request) ->
    case gen_restful_api:authorize_key_request(Request) of
        {error, no_key_configured} ->
            gen_restful_api:authenticate_admin_request(Request);
        deny ->
            deny;
        allow ->
            allow
    end.

run_command(Command, Args, Req) ->
    case ejabberd_commands:get_command_format(Command) of
        {error, _E} ->
            {error, bad_request};
        {ArgsF, ResF} ->
            case format_args(ArgsF, Args) of
                {error, Reason} ->
                    {error, Reason};
                ArgsFormatted ->
                    case ejabberd_commands:execute_command(Command, ArgsFormatted) of
                        {error, _Error} ->
                            {error, bad_request};
                        Result ->
                            format_result(ResF, Result, Req)
                    end
            end
    end.

format_args(ArgsF, Args) ->
    case catch [format_arg(ArgF, Arg) || {ArgF, Arg} <- lists:zip(ArgsF, Args)] of
        {'EXIT', _} ->
            {error, bad_request};
        ArgsFormatted ->
            ArgsFormatted
    end.

format_arg({_, integer}, Arg) ->
    list_to_integer(Arg);
format_arg({_, string}, Arg) ->
    Arg.

format_result(ResF, Res, #rest_req{format = json}) ->
    #rest_resp{format = json, output = format_result_json(Res, ResF)};
format_result(_ResF, _Res, #rest_req{format = xml}) ->
    % FIXME not implemented
    {error, bad_request}.

format_result_json({error, ErrorAtom}, _) ->
    [{error, ErrorAtom}];
format_result_json(Atom, {_, atom}) ->
    Atom;
format_result_json(Int, {_, integer}) ->
    integer_to_list(Int);
format_result_json(String, {_, string}) ->
    list_to_binary(String);
format_result_json(Code, {_, rescode}) ->
    Code;
format_result_json({Code, Text}, {_, restuple}) ->
    [{Code, list_to_binary(Text)}];
format_result_json([E], {_, {list, ElementsF}}) ->
    [format_result_json(E, ElementsF)];
format_result_json([E|T], {X, {list, ElementsF}}) ->
    [format_result_json(E, ElementsF) | format_result_json(T, {X, {list, ElementsF}}) ];
format_result_json(Tuple, {_, {tuple, ElementsF}}) ->
    TupleL = tuple_to_list(Tuple),
    % format a tuple as a list
    [format_result_json(E, F) || {E, F} <- lists:zip(TupleL, ElementsF)].

