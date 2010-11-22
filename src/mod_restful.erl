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
        process/2,

        % utilities
        simple_response/2
    ]).

-include("ejabberd.hrl").

-include("mod_restful.hrl").

-define(DEFAULT_FORMAT, json).
-define(ALLOWED_FORMATS, [json, xml]).

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
            case parse_http_request(HTTPRequest) of
                {error, Reason} ->
                    {reply,
                        #rest_resp{
                            status = http_status(Reason),
                            format = raw,
                            output = []
                        },
                        State};
                {ok, Format, Data} ->
                    Request = #rest_req{
                        path = Path,
                        host = HTTPRequest#request.host,
                        format = Format,
                        data = Data,
                        options = Opts,
                        http_request = HTTPRequest
                    },

                    case Module:process(Request) of
                        {error, Reason} ->
                            {reply, error_response(Reason, Request), State};
                        {simple, Response} ->
                            {reply, simple_response(Response, Request), State};
                        Response ->
                            {reply, Response, State}
                    end
            end;
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

simple_response(Atom, #rest_req{format = Format} = Request) ->
    case format_simple_response(Format, Atom) of
        {ok, Output} ->
            #rest_resp{
                status = 200,
                format = Format,
                output = Output
            };
        {error, Reason} ->
            mod_restful:error_response(Reason, Request)
    end.

format_simple_response(json, Atom) ->
    {ok, Atom};
format_simple_response(xml, Atom) ->
    {ok, {xmlelement, atom_to_list(Atom), [], []}}.

error_response(Reason, #rest_req{format = Format}) ->
    #rest_resp{
        status = http_status(Reason),
        format = Format,
        output = resp_error_output(Reason, Format)
    }.

http_status(bad_request) -> 400;
http_status(not_allowed) -> 401;
http_status(not_found) -> 404;
http_status(_) -> 403.

error_reason(bad_request) -> bad_request;
error_reason(not_allowed) -> unauthorized;
error_reason(not_found) -> not_found;
error_reason(Reason) -> Reason.

resp_error_output(Reason, xml) ->
    {xmlelement,
        "error",
        [{"reason", atom_to_list(error_reason(Reason))}],
        []};
resp_error_output(Reason, json) ->
    [{error, error_reason(Reason)}].

parse_http_request(#request{method = 'GET', q = Q}) ->
    case lists:keysearch("format", 1, Q) of
        {value, {_, Format}} ->
            FormatA = list_to_atom(Format),
            case lists:member(FormatA, ?ALLOWED_FORMATS) of
                true ->
                    {ok, FormatA, undefined};
                _ ->
                    {error, bad_request}
            end;
        _ ->
            {ok, ?DEFAULT_FORMAT, undefined}
    end;
parse_http_request(#request{method = 'POST'} = Request) ->
    parse_http_data(get_content_type(Request), Request).

get_content_type(#request{headers = Headers}) ->
    case lists:keysearch('Content-Type', 1, Headers) of
        {value, {_, ContentType}} ->
            ContentType;
        _ ->
            undefined
    end.

-spec parse_http_data(string(), #request{}) -> {error, bad_request} | {ok, json, term()}.
parse_http_data("application/json", #request{data = Data}) ->
    case catch mod_restful_mochijson2:decode(Data) of
        {'EXIT', _} ->
            {error, bad_request};
        JSON ->
            {ok, json, JSON}
    end;
parse_http_data("application/xml", _Request) ->
    % FIXME not implemented yet
    {error, bad_request};
parse_http_data(_, _Data) ->
    {error, bad_request}.

post_process(#rest_resp{
        status = Status,
        format = Format,
        headers = Headers,
        output = Output
    }) ->
    Headers1 = lists:keystore(?RESTFUL_CONTENTTYPE, 1, Headers,
        {?RESTFUL_CONTENTTYPE, content_type(Format)}),
    {Status, Headers1, encode(Format, Output)}.

content_type(json) -> "application/json";
content_type(xml) -> "application/xml";
content_type(raw) -> "text/plain".

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

