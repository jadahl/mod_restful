%%%----------------------------------------------------------------------
%%% File    : mod_restful.hrl
%%% Author  : Jonas Ådahl <jadahl@gmail.com>
%%% Purpose : Provides a RESTful API via http
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

-ifndef(mod_restful_hrl).
-define(mod_restful_hrl, true).

-include_lib("ejabberd/include/ejabberd_http.hrl").

-record(rest_req, {
        http_request :: #request{},
        path :: [string()],
        host :: string(),
        format :: json | xml,
        data :: any(),
        options :: list(),
        global_options :: list()
    }).

-record(rest_resp, {
        status = 200 :: integer(),
        format = json :: json | xml | raw,
        headers = [] :: [{iolist(), iolist()}],
        output :: any() % depends on format
    }).

-define(RESTFUL_CONTENTTYPE, <<"Content-Type">>).

-endif. % mod_restful_hrl
