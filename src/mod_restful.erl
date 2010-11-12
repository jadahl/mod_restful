%%%----------------------------------------------------------------------
%%% File    : mod_restful.erl
%%% Author  : Jonas Ådahl <jadahl@gmail.com>
%%% Purpose : Provides a RESTful API via http
%%% Created : 4 Nov 2010 by Jonas Ådahl <jadahl@gmail.com>
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

-module(mod_restful).
-author('jadahl@gmail.com').

-behaviour(gen_mod).
-behaviour(gen_server).

-export(
    [
        % gen_mod
        start/2, start_link/2, stop/1,

        % gen_server
        init/1, handle_call/3, handle_cast/2, handle_info/2,
        code_change/3, terminate/2,

        % http callback
        process/2
    ]).

-include("ejabberd.hrl").
%-include("web/ejabberd_http.hrl").

-include("mod_restful.hrl").

-type api_spec() :: {[string()], module(), list()}.

-record(state, {
        key :: undefined | string(),
        api = [] :: [api_spec()]
    }).

%
% gen_mod API
%

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    ChildSpec = {
        Proc,
        {?MODULE, start_link, [Host, Opts]},
        transient,
        1000,
        worker,
        [?MODULE]
    },
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%
% startup
%

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

%
% gen_server callbacks
%

init([_Host, Opts]) ->
    case lists:keysearch(api, 1, Opts) of
        {value, {_, API}} when is_list(API) and (API /= []) ->
            {ok, #state{api = API}};
        _ ->
            {stop, no_api}
    end.

handle_call({process, Path, HTTPRequest}, _From, #state{api = API} = State) ->
    case lists:keysearch([hd(Path)], 1, API) of
        {value, {_, Module, Opts}} ->
            Request = #rest_req{
                http_request = HTTPRequest,
                options = Opts
            },
            Response = Module:process(Request),
            {reply, Response, State};
        _R ->
            ?INFO_MSG("No API found: ~p (api=~p)", [_R, API]),
            {reply, {error, not_found}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%
% ejabberd_http API
%

process(BasePath, #request{host = Host, path = Path} = Request) ->
    case tl(Path) of
        [] ->
            ejabberd_web:error(not_found);
        _ ->
            case lists:member(Host, ejabberd_config:get_global_option(hosts)) of
                true ->
                    Proc = gen_mod:get_module_proc(Host, ?MODULE),
                    case gen_server:call(Proc, {process, BasePath, Request}) of
                        {error, Error} ->
                            ejabberd_web:error(Error);
                        Response ->
                            post_process(Response)
                    end;
                _ ->
                    ejabberd_web:error(not_found)
            end
    end.

%
% Internal
%

post_process(#rest_resp{
        status = Status,
        format = Format,
        headers = Headers,
        output = Output
    }) ->
    {Status, Headers, encode(Format, Output)}.

encode(raw, Output) ->
    Output;
encode(json, Output) ->
    encode_json(Output);
encode(xml, Output) ->
    encode_xml(Output).

encode_json(Output) ->
    mod_restful_mochijson2:encode(Output).

encode_xml(Output) ->
    xml:element_to_binary(Output).

