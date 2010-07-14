-module(ts_bosh).

-export([ connect/3, send/3, close/1, set_opts/2, protocol_options/1 ]).

-behaviour(gen_ts_transport).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(CONTENT_TYPE, "text/xml; charset=utf-8").
-define(VERSION, "1.8").
-define(WAIT, 60). %1 minute
-define(HOLD, 1). %only 1 request pending

-define(CONNECT_TIMEOUT, 20 * 1000).
-define(MAX_QUEUE_SIZE, 5). %% at most 5 messages queued, after that close the connection.
			    %% In practice we never had more than 1 pending packet, as we are blocking
			    %% the client process until we sent the packet.  But I keep this functionality in place,
			    %% in case we decide to do the sending() of data asynchronous.

-record(state, {
            host,
	    path,
	    port,
           % {Host::string(), Port:integer(), Path::string(), Ssl::bool()}
            domain = undefined,
            sid,
            rid,
            parent_pid,
            max_requests, %TODO: use this, now fixed on 2
            queue = [], %% stanzas that have been queued because we reach the limit of requets
            open = [],
            free = [],
            local_ip,
            local_port,
            is_fresh = true,
	    pending_ref
            }).

connect(Host, Port, Opts) ->
    Parent = self(),
    Path = "/http-bind/",
    Pid = spawn_link(fun() -> loop(Host, Port, Path, Opts, Parent) end),
    {ok, Pid}.

extract_domain("to='" ++ Rest) ->
	lists:takewhile(fun(C) -> C =/= $' end, Rest);
extract_domain([_|Rest]) -> extract_domain(Rest).
	
send(Pid, Data, _Opts) ->
    Ref = make_ref(),
    Msg = case Data of
        <<"<?xml", Rest/binary>> ->  %%HACK: use this to detect stream start (or restarts)
            Domain =  extract_domain(binary_to_list(Rest)), 
            {stream, Domain, Ref};
	<<"</stream:stream>", _/binary>> -> %%Use this to detect stream end
	    {stream, terminate, Ref};
        _ ->
            {send, Data, Ref}
    end,
    Pid ! Msg,
    MonitorRef = erlang:monitor(process,Pid),
    receive
	{'DOWN', MonitorRef, _Type, _Object, _Info} ->
		{error, no_bosh_connection};
        {ok, Ref} -> 
		erlang:demonitor(MonitorRef),
		ok
    after
        30000 ->
	    erlang:demonitor(MonitorRef),
            {error, timeout}
    end.

close(Pid) ->
    Pid ! close.

set_opts(Pid, _Opts) ->
    Pid.

protocol_options(#proto_opts{}) ->
    [].

loop(Host, Port, Path, Opts, Parent) ->
   {A,B,C} = now(),
   random:seed(A,B,C),
   process_flag(trap_exit, true),
   loop(#state{is_fresh = true, 
   		port = Port,
		path = Path, 
		parent_pid = Parent,
		host = Host, 
		local_ip = proplists:get_value(ip, Opts, undefined),
		local_port = proplists:get_value(port, Opts, undefined)
		}).

loop(#state{parent_pid = ParentPid} = State) ->
    receive
	{'EXIT', ParentPid, _Reason} -> %%even 'normal' terminates this
  	    ok; 
        close ->
	    ok;
       {send, Data, Ref}  ->
            case  do_send(State, Data) of
		{sent, NewState} ->
	            ParentPid ! {ok, Ref},
		    loop(NewState);
		{queued, #state{queue =Q} = NewState} when length(Q) < ?MAX_QUEUE_SIZE ->  %%do not return yet..
		     loop(NewState#state{pending_ref = Ref});
		{queued, NewState} -> %% we reach the max allowed queued messages.. close the connection.
		     ?LOGF("Client reached max bosh requests queue size: ~p. Closing session", 
			[length(NewState#state.queue)], ?ERR),
		     ts_mon:add({count, error_bosh_maxqueued}),
	             ParentPid ! {ok, Ref},
		     ParentPid ! {gen_ts_transport, self(), closed}
	    end;
       {stream, terminate, Ref} ->
	    #state{host = Host,
		   path = Path,
		   sid = Sid,
		   rid = Rid} = State,
            {NewState, Socket} = new_socket(State, false),
            ok = make_raw_request(Socket, Host, Path, close_stream_msg(Sid, Rid)),
            ParentPid ! {ok, Ref},
	    loop(NewState);
       {stream, Domain, Ref} when State#state.domain == undefined ->
	    NewState = do_connect(State, Domain),
            ParentPid ! {ok, Ref},
	    loop(NewState);
       {stream, _Domain, Ref} -> %%here we must do a reset
	    NewState = do_reset(State),
            ParentPid ! {ok, Ref},
	    loop(NewState);
       {http, Socket, {http_response, Vsn, 200, <<"OK">>}} ->
       		case do_receive_http_response(State, Socket, Vsn) of
			{ok, NewState} ->
				loop(NewState);
			terminate ->
				?LOG("Session terminated by server", ?INFO),
            			State#state.parent_pid ! {gen_ts_transport, self(), closed}
		end;
	{tcp_closed, Socket} ->
		case lists:keymember(Socket, 1, State#state.open) of
			true ->
				%%ERROR, a current request is closed
				?LOG("Open request closed by server", ?ERR),
				ts_mon:add({count, error_bosh_socket_closed}),
            			State#state.parent_pid ! {gen_ts_transport, self(), closed};
			false ->
				%% A HTTP persistent connection, currently not in use, is closed by the server.
				%% We can continue without trouble, just remove it, it will be reopened when needed.
				loop(State#state{free = lists:delete(Socket, State#state.free)})
		end;
       {http, _Socket, {http_response, _Vsn, ResponseCode, _StatusLine}} ->
		State#state.parent_pid ! {gen_ts_transport, self(), error, list_to_atom(integer_to_list(ResponseCode))};
        Unexpected ->
		?LOGF("Bosh process received unexpected message: ~p", [Unexpected], ?ERR),
		State#state.parent_pid ! {gen_ts_transport, self(), error, unexpected_data}
   end.
    
do_receive_http_response(State, Socket, Vsn) ->
	#state{open = Open,
		sid = Sid,
		rid = Rid,
		queue = Queue,
		host = Host,
		path = Path,
		parent_pid = ParentPid} = State,
	{ok, {{200, <<"OK">>}, _Hdrs, Resp}} = read_response(Socket, Vsn, {200, <<"OK">>}, [], <<>>),
	{_El = #xmlElement{name = body, 
		attributes = Attrs,
		content = Content}, []}= xmerl_scan:string(binary_to_list(Resp)),
	case get_attr(Attrs, type) of
		"terminate" ->
			ts_mon:add({count, error_bosh_terminated}),
			State#state.parent_pid ! {gen_ts_transport, self(), closed},
			terminate;
		_ ->
		NewOpen = lists:keydelete(Socket, 1, Open),
		NewState2  = if
			     NewOpen == [] andalso State#state.is_fresh =:= false ->
				inet:setopts(Socket, [{packet, http_bin}, {active, once}]),
				ok = make_empty_request(Socket,Sid, Rid, Queue, Host, Path),
				case length(Queue) of
					0 -> ok;
					_ -> 
					ParentPid ! {ok, State#state.pending_ref}
					   %% we just sent the pending packet, wakeup the client
				end,
				State#state{open = [{Socket, Rid}], rid = Rid +1, queue = []};
			     length(NewOpen) == 1 andalso length(State#state.queue) > 0 ->
				%%there are pending packet, sent it if the RID is ok, otherwise wait
				case NewOpen of
					[{_, R}] when (Rid - R) =< 1 ->
						inet:setopts(Socket, [{packet, http_bin}, {active, once}]),
						ok = make_empty_request(Socket,Sid, Rid, Queue, Host, Path),
						ParentPid ! {ok, State#state.pending_ref},
						%% we just sent the pending packet, wakeup the client
						State#state{open = [{Socket, Rid}], rid = Rid +1, queue = []};
					_ ->
						NewState = return_socket(State, Socket),
						NewState#state{open = NewOpen}
				end;
			     true ->
				NewState = return_socket(State, Socket),
				NewState#state{open = NewOpen}
		end,
		case Content of
			[] -> 
				%%empty response, do not bother the ts_client process with this
				%% (so Noack/Bidi won't count this bosh specific thing, only async stanzas)
				%% since ts_client don't see this, we need to count the size received
				ts_mon:add({ sum, size_rcv, iolist_size(Resp)});
			_ ->	
			ParentPid ! {gen_ts_transport, self(), Resp}
		end,
		{ok, NewState2}
	end.

do_connect(#state{host = Host, path = Path, parent_pid = ParentPid} = State, Domain) ->
    Rid = 1000 + random:uniform(100000),
    %%Port= proplists:get_value(local_port, Options, undefined),
    NewState = State#state{
            domain = Domain,
            rid = Rid,
            open = [],
            queue = [],
            free = []
            },
    {NewState2, Socket} = new_socket(NewState, false),
    ok = make_raw_request(Socket, Host, Path, create_session_msg(Rid, Domain, ?WAIT, ?HOLD)),
    {ok, {{200, <<"OK">>}, _Hdrs, Resp}} = read_response(Socket, nil, nil, [], <<>>),
    NewState3 = return_socket(NewState2, Socket),
    {_El = #xmlElement{name = body,
	attributes = Attrs,
	content = _Content}, []} = xmerl_scan:string(binary_to_list(Resp)),
    ParentPid ! {gen_ts_transport, self(), Resp},
    NewState3#state{rid = Rid +1,
	    open = [],
            sid = get_attr(Attrs, sid),
            max_requests = 2
            }.

do_reset(State) ->
    #state{sid = Sid,
           rid = Rid,
           host = Host,
	   path = Path,
           domain = Domain} =  State,
    {NewState, Socket} = new_socket(State, once),
    ok = make_raw_request(Socket, Host, Path, restart_stream_msg(Sid, Rid, Domain)),
    NewState#state{is_fresh = false, rid = Rid +1, open = [{Socket, Rid}|State#state.open]}.

get_attr([], _Name) -> undefined;
get_attr([#xmlAttribute{name = Name, value = Value}|_], Name) -> Value;
get_attr([_|Rest], Name) -> get_attr(Rest, Name).

do_send(State, Data) ->
   #state{open = Open,
          rid = Rid,
          sid = Sid,
          host = Host,
	  path = Path,
          queue = Queue} = State,
    Result = if
                Open == []  -> send;
                true ->
                        Min = lists:min(lists:map(fun({_S,R}) -> R end, Open)),
                        if
                                (Rid -Min) =< 1 ->
                                        send;
                                true ->
                                        queue
                        end
            end,
     case Result of
         send ->
              {NewState, Socket} = new_socket(State, once),
              ok = make_request(Socket, Sid, Rid, Queue, Host, Path, Data),
              {sent, NewState#state{rid = Rid +1, open = [{Socket, Rid}|Open], queue = []}};
         queue ->
                Queue = State#state.queue,
                NewQueue =  [Data|Queue],
               {queued, State#state{queue = NewQueue}}
    end.

make_empty_request(Socket, Sid, Rid, Queue, Host, Path) ->
    StanzasText = lists:reverse(Queue),
    Body = stanzas_msg(Sid, Rid, StanzasText),
    make_request(Socket, Host, Path, Body, iolist_size(StanzasText)).

make_raw_request(Socket, Host, Path, Body) ->
    make_request(Socket, Host, Path, Body, 0).

make_request(Socket, Sid, Rid, Queue, Host, Path, Packet) ->
    StanzasText = lists:reverse([Packet|Queue]),
    Body = stanzas_msg(Sid, Rid, StanzasText),
    make_request(Socket, Host, Path, Body, iolist_size(StanzasText)).                                                       
make_request(Socket,Host, Path, Body, OriginalSize) ->
     ts_mon:add({count, bosh_http_req}),
     Hdrs = [{"Content-Type", ?CONTENT_TYPE}, {"keep-alive", "true"}],
     Request = format_request(Path, "POST", Hdrs, Host, Body),
     ok = gen_tcp:send(Socket, Request),
     ts_mon:add({ sum, size_sent, iolist_size(Request) - OriginalSize}).
     %% add the http overhead. The size of the stanzas are already counted by ts_client code.


new_socket(State = #state{free = [Socket | Rest]}, Active) ->
        inet:setopts(Socket, [{active, Active}, {packet, http_bin}]),
        {State#state{free = Rest}, Socket};
new_socket(State = #state{host = Host, port = Port, local_ip = LocalIp, local_port = LocalPort}, Active) ->
    Options = case LocalIp of
                        undefined -> [{active, Active}, {packet, http_bin}];
                        _ ->  case LocalPort of
                                undefined -> [{active, Active}, {packet, http_bin},{ip, LocalIp}];
                                _ -> 
				{ok, LPort} = ts_config_server:get_user_port(LocalIp),
				[{active, Active}, {packet, http_bin},{ip, LocalIp}, {port, LPort}]
                              end
    end,
    {ok, Socket} = gen_tcp:connect(Host, Port,  Options, ?CONNECT_TIMEOUT),
    ts_mon:add({count, bosh_http_conn}),
    {State, Socket}.

return_socket(State, Socket) ->
        inet:setopts(Socket, [{active, once}]), 
	%receive data from it, we want to know if something happens
        State#state{free = [Socket | State#state.free]}.

create_session_msg(Rid, To, Wait, Hold) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind'"
       " content='text/xml; charset=utf-8'",
       " ver='1.8'"
       " to='", To, "'",
       " rid='", integer_to_list(Rid), "'"
       " xmlns:xmpp='urn:xmpp:xbosh'",
       " xmpp:version='1.0'",
       " wait='", integer_to_list(Wait), "'"
       " hold='", integer_to_list(Hold), "'/>"].

stanzas_msg(Sid, Rid, Text) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'"
       " sid='", Sid, "'>", Text, "</body>"].

restart_stream_msg(Sid, Rid, Domain) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'",
       " sid='", Sid, "'",
       " xmpp:restart='true'",
       " xmlns:xmpp='urn:xmpp:xbosh'",
       " to='", Domain, "'",
       "/>"].

close_stream_msg(Sid, Rid) ->
    [ "<body xmlns='http://jabber.org/protocol/httpbind' "
       " rid='", integer_to_list(Rid), "'",
       " sid='", Sid, "'",
       " type='terminate'",
       " xmlns:xmpp='urn:xmpp:xbosh'",
       "/>"].

read_response(Socket, Vsn, Status, Hdrs, Body) ->
    inet:setopts(Socket, [{packet, http_bin}, {active, false}]),
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_response, NewVsn, StatusCode, Reason}} ->
            NewStatus = {StatusCode, Reason},
            read_response(Socket, NewVsn, NewStatus, Hdrs, Body);
        {ok, {http_header, _, Name, _, Value}} ->
            Header = {Name, Value},
            read_response(Socket, Vsn, Status, [Header | Hdrs], Body);
        {ok, http_eoh} ->
            inet:setopts(Socket, [{packet, raw}, binary]),
            {NewBody, NewHdrs} = read_body(Vsn, Hdrs, Socket),
            Response = {Status, NewHdrs, NewBody},
            {ok, Response};
        {error, closed} ->
            erlang:error(closed);
        {error, Reason} ->
            erlang:error(Reason)
    end.

read_body(_Vsn, Hdrs, Socket) ->
    % Find out how to read the entity body from the request.
    % * If we have a Content-Length, just use that and read the complete
    %   entity.
    % * If Transfer-Encoding is set to chunked, we should read one chunk at
    %   the time
    % * If neither of this is true, we need to read until the socket is
    %   closed (AFAIK, this was common in versions before 1.1).
    case proplists:get_value('Content-Length', Hdrs, undefined) of
        undefined ->
                throw({no_content_length, Hdrs});
        ContentLength ->
            read_length(Hdrs, Socket, list_to_integer(binary_to_list(ContentLength)))
    end.

read_length(Hdrs, Socket, Length) ->
    case gen_tcp:recv(Socket, Length) of
        {ok, Data} ->
            {Data, Hdrs};
        {error, Reason} ->
            erlang:error(Reason)
    end.

%% @spec (Path, Method, Headers, Host, Body) -> Request
%% Path = iolist()
%% Method = atom() | string()
%% Headers = [{atom() | string(), string()}]
%% Host = string()
%% Body = iolist()
format_request(Path, Method, Hdrs, Host, Body) ->
    [
        Method, " ", Path, " HTTP/1.1\r\n",
        format_hdrs(add_mandatory_hdrs(Method, Hdrs, Host, Body), []),
        Body
    ].

%% spec normalize_method(AtomOrString) -> Method
%%   AtomOrString = atom() | string()
%%   Method = string()
%% doc
%% Turns the method in to a string suitable for inclusion in a HTTP request
%% line.
%% end
%-spec normalize_method(atom() | string()) -> string().
%normalize_method(Method) when is_atom(Method) ->
%    string:to_upper(atom_to_list(Method));
%normalize_method(Method) ->
%    Method.

format_hdrs([{Hdr, Value} | T], Acc) ->
    NewAcc = [
        Hdr, ":", Value, "\r\n" | Acc
    ],
    format_hdrs(T, NewAcc);
format_hdrs([], Acc) ->
    [Acc, "\r\n"].

add_mandatory_hdrs(Method, Hdrs, Host, Body) ->
    add_host(add_content_length(Method, Hdrs, Body), Host).

add_content_length("POST", Hdrs, Body) ->
    add_content_length(Hdrs, Body);
add_content_length("PUT", Hdrs, Body) ->
    add_content_length(Hdrs, Body);
add_content_length(_, Hdrs, _) ->
    Hdrs.

add_content_length(Hdrs, Body) ->
    case proplists:get_value("content-length", Hdrs, undefined) of
        undefined ->
            ContentLength = integer_to_list(iolist_size(Body)),
            [{"Content-Length", ContentLength} | Hdrs];
        _ -> % We have a content length
            Hdrs
    end.

add_host(Hdrs, Host) ->
    case proplists:get_value("host", Hdrs, undefined) of
        undefined ->
            [{"Host", Host } | Hdrs];
        _ -> % We have a host
            Hdrs
    end.
