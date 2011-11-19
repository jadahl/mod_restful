%%%----------------------------------------------------------------------
%%% File    : mod_restful.erl
%%% Author  : Jonas Ådahl <jadahl@gmail.com>
%%% Purpose : Tests for mod_restful
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

-module(mod_restful_tests).
-author('jadahl@gmail.com').

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("mod_restful.hrl").

-define(FORMATS, [json, xml]).

error_response_test() ->
    ReqJSON = #rest_req{format = json},
    ReqXML = #rest_req{format = xml},

    Errors = [
        {bad_request, 400, [{error, bad_request}], {xmlelement, "error", [], [{xmlcdata, "bad_request"}]}},
        {not_allowed, 401, [{error, not_allowed}], {xmlelement, "error", [], [{xmlcdata, "not_allowed"}]}},
        {not_found, 404, [{error, not_found}], {xmlelement, "error", [], [{xmlcdata, "not_found"}]}}
    ],

    [
        ?assertEqual(#rest_resp{format = json, status = Status, output = Output}, mod_restful:error_response(Error, ReqJSON)) || {Error, Status, Output, _} <- Errors
    ],

    [
        ?assertEqual(#rest_resp{format = xml, status = Status, output = Output}, mod_restful:error_response(Error, ReqXML)) || {Error, Status, _, Output} <- Errors
    ],

    ok.

simple_response_test() ->
    Tests = [
        {json, [
                % basic
                {ok, ok},
                {foo, foo},
                {true, true},
                {123, 123},

                % structs
                {{[{key, value}]}, [{key, value}]},
                {{[{anotherkey, 52}]}, [{anotherkey, 52}]}
            ]},
        {xml, [
                % basic
                {ok, {xmlelement, "ok", [], []}},
                {foo, {xmlelement, "foo", [], []}},
                {true, {xmlelement, "true", [], []}},
                {123, {xmlelement, "value", [], [{xmlcdata, <<"123">>}]}},

                % structs
                {{[{key, value}]}, {xmlelement, "struct", [],
                                    [{xmlelement, "entry", [{"key", "key"}],
                                     [{xmlcdata, "value"}]}]}}
            ]}
    ],

    Fail = [
        {ok, ok},
        [a, b, c]
    ],

    [
        [
            ?assertEqual(
                #rest_resp{status = 200, format = Format, output = Out},
                mod_restful:simple_response(In, #rest_req{format = Format}))
            ||
            {In, Out} <- TestList
        ]
        ||
        {Format, TestList} <- Tests
    ],

    [
        [
            ?assertError(_, mod_restful:simple_response(In, #rest_req{format = Format}))
            ||
            In <- Fail
        ]
        ||
        Format <- ?FORMATS
    ],

    ok.

