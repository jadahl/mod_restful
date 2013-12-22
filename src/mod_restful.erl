%%%----------------------------------------------------------------------
%%% File    : mod_restful.erl
%%% Author  : Jonas Ådahl <jadahl@gmail.com>
%%% Purpose : Provides a RESTful API via http
%%% Created : 4 Nov 2010 by Jonas Ådahl <jadahl@gmail.com>
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
        error_response/2,
        simple_response/2
    ]).

-ifdef(test).
-export(
    [
        parse_http_request/1
    ]).
-endif.

-include_lib("ejabberd/include/ejabberd.hrl").

-include("mod_restful.hrl").

-define(DEFAULT_FORMAT, json).
-define(ALLOWED_FORMATS, [json, xml]).

-type api_spec() :: {[string()], module(), list()}.

-record(state, {
        options = [] :: [{atom(), term()}],
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
    catch gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

%
% Startup
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
            Options = proplists:delete(api, Opts),
            {ok, #state{api = API, options = Options}};
        _ ->
            {stop, no_api}
    end.

get_api_mod(_Path, []) ->
    undefined;
get_api_mod(Path, [Mod | API]) ->
    case proplists:get_value(path, Mod) of
        Path -> Mod;
        _ -> get_api_mod(Path, API)
    end.

handle_call({get_spec, Path}, _From, #state{api = API,
                                            options = GlobalOpts} = State) ->
    case get_api_mod([hd(Path)], API) of
        undefined ->
            error_logger:warning_msg("API module for path ~p not found~n",
                                     [hd(Path)]),
            {reply, {error, not_found}, State};
        Mod ->
            Module = proplists:get_value(module, Mod),
            Opts = proplists:get_value(params, Mod),
            {reply, {ok, Module, Opts, GlobalOpts}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
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
    try
        case tl(Path) of
            [] ->
                ejabberd_web:error(not_found);
            _ ->
                post_process(handle_request(Host, BasePath, Request))
        end
    catch
        _ = Error ->
            error_logger:error_msg("Processing throwed error ~p~ntrace: ~p~n",
                                   [Error, erlang:get_stacktrace()]),
            ejabberd_web:error(not_allowed);
        _:_ = Error ->
            error_logger:error_msg("Processing throwed error ~p~ntrace: ~p~n",
                                   [Error, erlang:get_stacktrace()]),
            ejabberd_web:error(not_allowed)
    end.

%
% Internal
%

handle_request(Host, BasePath, Request) ->
    true = lists:member(Host, ejabberd_config:get_global_option(hosts, fun(V) -> V end)),
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    {ok, Module, Opts, GlobalOpts} =
        gen_server:call(Proc, {get_spec, BasePath}),
    handle_rest_request(Module, BasePath, Opts, GlobalOpts, Request).

handle_rest_request(Module, Path, Opts, GlobalOpts, HTTPRequest) ->
    try
        {ok, Format, Data} = parse_http_request(HTTPRequest),
        Request = #rest_req{path = Path, options = Opts,
                            global_options = GlobalOpts,
                            format = Format, data = Data,
                            host = HTTPRequest#request.host,
                            http_request = HTTPRequest},
        process_reply(Module:process_rest(Request), Request)
    catch
        {error, Reason} = Error when is_atom(Reason) ->
            error_logger:error_msg("Processing throwed error ~p~ntrace: ~p~n",
                                   [Error, erlang:get_stacktrace()]),
            {error, Reason};
        _:_ = Error ->
            error_logger:error_msg("Processing throwed error ~p~ntrace: ~p~n",
                                   [Error, erlang:get_stacktrace()]),
            {error, bad_request}
    end.

%
% Output processing
%

process_reply(Reply, Request) ->
    Response = case Reply of
        {simple, Simple} -> simple_response(Simple, Request);
        {error, Reason}  -> error_response(Reason, Request);
        {ok, Response1}  -> Response1
    end,

    #rest_resp{status = Status,
               format = Format,
               headers = Headers,
               output = Output} = Response,

    Headers1 = lists:keystore(?RESTFUL_CONTENTTYPE, 1, Headers,
        {?RESTFUL_CONTENTTYPE, content_type(Format)}),
    {Status, Headers1, encode(Format, Output)}.

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

%
% Format JSON
%
format_simple_response(json, Simple) when is_atom(Simple) or
                                          is_number(Simple) ->
    {ok, Simple};
format_simple_response(json, String) when is_list(String) ->
    {ok, list_to_binary(String)};
format_simple_response(json, Binary) when is_binary(Binary) ->
    {ok, Binary};
format_simple_response(json, {Entries}) when is_list(Entries) ->
    {ok, [format_simple_json_struct_entry(Entry) || Entry <- Entries]};

%
% Format XML
%
format_simple_response(xml, Atom) when is_atom(Atom) ->
    {ok, {xmlelement, atom_to_list(Atom), [], []}};
format_simple_response(xml, Value) when is_number(Value) or is_list(Value) ->
    Out = if
        is_integer(Value) -> integer_to_list(Value);
        is_float(Value)   -> float_to_list(Value);
        is_list(Value)    -> Value
    end,
    OutB = list_to_binary(Out),
    {ok, {xmlelement, "value", [], [{xmlcdata, OutB}]}};
format_simple_response(xml, {Entries}) when is_list(Entries) ->
    {ok, {xmlelement, "struct", [],
          [format_simple_xml_struct_entry(Entry) || Entry <- Entries]}}.

format_simple_json_struct_entry({Key, Value}) when is_atom(Key) ->
    {Key, Value}.

to_list(Float) when is_float(Float)       -> float_to_list(Float);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Atom) when is_atom(Atom)          -> atom_to_list(Atom).

format_simple_xml_struct_entry({Key, Value}) when
  is_atom(Key), (is_number(Value) or is_atom(Value)) ->
    {xmlelement, "entry", [{"key", to_list(Key)}],
     [{xmlcdata, to_list(Value)}]}.

error_response(Reason, #rest_req{format = Format}) when is_atom(Reason) ->
    #rest_resp{
        status = http_status(Reason),
        format = Format,
        output = resp_error_output(Reason, Format)
    }.

http_status(bad_request) -> 400;
http_status(not_allowed) -> 401;
http_status(not_found) -> 404;
http_status(_) -> 403.

resp_error_output(Reason, xml) ->
    {xmlelement,
        "error",
        [],
        [{xmlcdata, atom_to_list(Reason)}]};
resp_error_output(Reason, json) ->
    [{error, Reason}].

post_process({error, not_found})   -> ejabberd_web:error(not_found);
post_process({error, not_allowed}) -> ejabberd_web:error(not_allowed);
post_process({error, Reason})      -> {http_status(Reason), [], []};
post_process(Result)               -> Result.

content_type(json) -> <<"application/json">>;
content_type(xml) -> <<"application/xml">>;
content_type(raw) -> <<"text/plain">>.

to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(List) when is_list(List)       -> list_to_binary(List).

encode(raw, Output) ->
    Output;
encode(json, Output) ->
    encode_json(Output);
encode(xml, Output) ->
    encode_xml(Output).

encode_json(Output) ->
    to_binary(mod_restful_mochijson2:encode(Output)).

encode_xml(Output) ->
    xml:element_to_binary(Output).

%
% Parsing
%

parse_http_request(#request{method = 'GET', q = Q}) ->
    case lists:keysearch(<<"format">>, 1, Q) of
        {value, {_, Format}} ->
            FormatA = list_to_atom(binary_to_list(Format)),
            case lists:member(FormatA, ?ALLOWED_FORMATS) of
                true ->
                    {ok, FormatA, undefined};
                _ ->
                    error_logger:warning_msg("Format ~s not allowed~n", [Format]),
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

-spec parse_http_data(string(), #request{}) ->
    {error, bad_request} | {ok, json, term()}.
parse_http_data(<<"application/json">>, #request{data = Data}) ->
    JSON = mod_restful_mochijson2:decode(Data),
    {ok, json, JSON};
parse_http_data(<<"application/xml">>, _Request) ->
    % FIXME not implemented yet
    error_logger:warning_msg("XML support not implemented yet~n"),
    {error, bad_request};
parse_http_data(_Format, _Data) ->
    error_logger:warning_msg("Unknown format ~s~n", [_Format]),
    {error, bad_request}.

