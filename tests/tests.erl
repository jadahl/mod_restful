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

-module(tests).

-include_lib("eunit/include/eunit.hrl").

-define(TESTS, [
        mod_restful_tests,
        mod_restful_admin_tests,
        mod_restful_register_tests
    ]).

all_test() ->
    lists:foreach(fun(Test) -> eunit:test(Test, [verbose]) end, ?TESTS).

