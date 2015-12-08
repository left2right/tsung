%%%
%%%  Copyright © IDEALX S.A.S. 2004
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 20 Apr 2004 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_config_msync).
-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-export([parse_config/2 ]).

-include("ts_profile.hrl").
-include("ts_msync.hrl").
-include("ts_config.hrl").

-include("xmerl.hrl").


%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% TODO: Dynamic content substitution is not yet supported for Jabber
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=msync},
             Config=#config{curid= Id, session_tab = Tab,
                            match=MatchRegExp, dynvar=DynVar,
                            subst= SubstFlag, sessions = [CurS |_]}) ->

    initialize_options(Tab),

    TypeStr  = ts_config:getAttr(string,Element#xmlElement.attributes, type, "chat"),
    Ack  = ts_config:getAttr(atom,Element#xmlElement.attributes, ack, no_ack),
    Dest= ts_config:getAttr(atom,Element#xmlElement.attributes, destination,random),
    Stamped = ts_config:getAttr(atom,Element#xmlElement.attributes, stamped, false),

    Size= ts_config:getAttr(integer,Element#xmlElement.attributes, size,0),
    Data= ts_config:getAttr(string,Element#xmlElement.attributes, data,undefined),
    Show= ts_config:getAttr(string,Element#xmlElement.attributes, show, "chat"),
    Status= ts_config:getAttr(string,Element#xmlElement.attributes, status, "Available"),
    Resource= ts_config:getAttr(string,Element#xmlElement.attributes, resource, "tsung"),
    Type= list_to_atom(TypeStr),
    Version = ts_config:getAttr(string,Element#xmlElement.attributes, version, "1.0"),
    Cacert = ts_config:getAttr(string,Element#xmlElement.attributes, cacertfile, undefined),
    KeyFile = ts_config:getAttr(string,Element#xmlElement.attributes, keyfile, undefined),
    KeyPass = ts_config:getAttr(string,Element#xmlElement.attributes, keypass, undefined),
    CertFile = ts_config:getAttr(string,Element#xmlElement.attributes, certfile, undefined),
    Room = ts_config:getAttr(string,Element#xmlElement.attributes, room, undefined),
    Nick = ts_config:getAttr(string,Element#xmlElement.attributes, nick, undefined),
    Group = ts_config:getAttr(string,Element#xmlElement.attributes, group, "Tsung Group"),
    RE = ts_config:getAttr(string,Element#xmlElement.attributes, regexp, undefined),
    Node = case ts_config:getAttr(string, Element#xmlElement.attributes, 'node', undefined) of
                    "" -> user_root;
                    X -> X
                end,
    NodeType = ts_config:getAttr(string, Element#xmlElement.attributes, 'node_type', undefined),
    %% This specify where the node identified in the 'node' attribute is located.
    %% If node is undefined  (no node attribute)
    %%    -> we don't specify the node, let the server choose one for us.
    %% else
    %%     If node is absolute (starts with "/")
    %%         use that absolute address
    %%     else
    %%        the address is relative. Composed of two variables: user and node
    %%        if node is "" (attribute node="")
    %%            we want the "root" node for that user (/home/domain/user)
    %%        else
    %%            we want a specific child node for that user (/home/domain/user/node)
    %%        in both cases, the user is obtained as:
    %%        if dest == "random"
    %%           random_user()
    %%        if dest == "online"
    %%            online_user()
    %%        if dest == "offline"
    %%            offline_user()
    %%        Otherwise:    (any other string)
    %%          The specified string
    SubId = ts_config:getAttr(string, Element#xmlElement.attributes, 'subid', undefined),

    Domain  =ts_config:get_default(Tab, msync_domain_name, msync_domain),
    ?LOGF("XMPP domain is ~p~n",[Domain],?DEB),

    MUC_service = ts_config:get_default(Tab, muc_service),
    PubSub_service =ts_config:get_default(Tab, pubsub_service),

    UserPrefix=ts_config:get_default(Tab, msync_username),
    UserIdMax = ts_config:get_default(Tab, msync_userid_max),

    %% Authentication
    {XMPPId, NewAppkey, UserName, Passwd} = case lists:keysearch(msync_authenticate, #xmlElement.name,
                                           Element#xmlElement.content) of
                          {value, AuthEl=#xmlElement{} } ->
                              Appkey = ts_config:getAttr(string,AuthEl#xmlElement.attributes,
                                                          appkey, undefined),
                              User= ts_config:getAttr(string,AuthEl#xmlElement.attributes,
                                                          username, undefined),
                              PWD= ts_config:getAttr(string,AuthEl#xmlElement.attributes,
                                                        passwd, undefined),
                              {user_defined,Appkey,User,PWD};
                          _ ->
                              GAppkey  =ts_config:get_default(Tab, msync_appkey),
                              GPasswd  =ts_config:get_default(Tab, msync_passwd),
                              {0,GAppkey,UserPrefix,GPasswd}
                      end,



    Msg=#ts_request{ack   = Ack,
                    dynvar_specs= DynVar,
                    endpage = true,
                    subst   = SubstFlag,
                    match   = MatchRegExp,
                    param = #msync{domain = Domain,
                                    appkey = NewAppkey,
                                    username = UserName,
                                    passwd = Passwd,
                                    id     = XMPPId,
                                    data   = Data,
                                    type   = Type,
                                    stamped = Stamped,
                                    regexp = RE,
                                    dest   = Dest,
                                    size   = Size,
                                    show   = Show,
                                    status   = Status,
                                    resource = Resource,
                                    room = Room,
                                    nick = Nick,
                                    group = Group,
                                    muc_service = MUC_service,
                                    pubsub_service = PubSub_service,
                                    node = Node,
                                    node_type = NodeType,
                                    subid = SubId,
                                    version = Version,
                                    cacertfile = Cacert,
                                    keyfile = KeyFile,
                                    keypass = KeyPass,
                                    certfile = CertFile,
                                    prefix = UserPrefix
                                   }
                   },
    ts_config:mark_prev_req(Id-1, Tab, CurS),
    ets:insert(Tab,{{CurS#session.id, Id}, Msg}),
    ?LOGF("Insert new request ~p, id is ~p~n",[Msg,Id],?INFO),
    lists:foldl( fun(A,B) -> ts_config:parse(A,B) end,
                 Config#config{dynvar=[], user_server_maxuid = UserIdMax},
                 Element#xmlElement.content);
%% Parsing options
parse_config(Element = #xmlElement{name=option}, Conf = #config{session_tab = Tab}) ->
    NewConf = case ts_config:getAttr(Element#xmlElement.attributes, name) of
        "appkey" ->
            Val = ts_config:getAttr(string,Element#xmlElement.attributes, value,?msync_appkey),
            ets:insert(Tab,{{msync_appkey,value}, Val}),
            Conf;
        "username" ->
            Val = ts_config:getAttr(string,Element#xmlElement.attributes, value,?msync_username),
            ets:insert(Tab,{{msync_username,value}, Val}),
            Conf;
        "passwd" ->
            Val = ts_config:getAttr(string,Element#xmlElement.attributes, value,?msync_passwd),
            ets:insert(Tab,{{msync_passwd,value}, Val}),
            Conf;
        "domain" ->
            Val = ts_config:getAttr(string,Element#xmlElement.attributes, value, ?msync_domain),
            ets:insert(Tab,{{msync_domain_name,value}, {domain,Val}}),
            Conf;
        "vhost_file" ->
            Val = ts_config:getAttr(atom,Element#xmlElement.attributes, value,"vhostfile"),
            ets:insert_new(Tab,{{msync_domain_name,value}, {vhost,Val}}),
            Conf#config{vhost_file = Val};
        "global_number" ->
            N = ts_config:getAttr(integer,Element#xmlElement.attributes, value, ?msync_global_number),
            ets:insert(Tab,{{msync_global_number, value}, N}),
            Conf;
        "userid_max" ->
            N = ts_config:getAttr(integer,Element#xmlElement.attributes, value, ?msync_userid_max),
            ts_user_server:reset(N),
            ets:insert(Tab,{{msync_userid_max,value}, N}),
            Conf#config{user_server_maxuid = N};
        "muc_service" ->
            N = ts_config:getAttr(string,Element#xmlElement.attributes, value, "conference.localhost"),
            ets:insert(Tab,{{muc_service,value}, N}),
            Conf;
        "pubsub_service" ->
            N = ts_config:getAttr(string,Element#xmlElement.attributes, value, "pubsub.localhost"),
            ets:insert(Tab,{{pubsub_service,value}, N}),
            Conf;
        "random_from_fileid" ->
            FileId = ts_config:getAttr(atom,Element#xmlElement.attributes, value, none),
            ?LOGF("set random fileid to  ~p~n",[FileId],?WARN),

            ts_user_server:set_random_fileid(FileId),
            Conf;
        "offline_from_fileid" ->
            FileId = ts_config:getAttr(atom,Element#xmlElement.attributes, value, none),
            ?LOGF("set offline fileid to  ~p~n",[FileId],?WARN),

            ts_user_server:set_offline_fileid(FileId),
            Conf;
        "fileid_delimiter" ->
            D = ts_config:getAttr(string,Element#xmlElement.attributes, value, ";"),
            ts_user_server:set_fileid_delimiter(list_to_binary(D)),
            Conf
    end,
    lists:foldl( fun(A,B) -> ts_config:parse(A,B) end, NewConf, Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

initialize_options(Tab) ->
    case ts_config:get_default(Tab, msync_initialized) of
        {undef_var,_} ->
            ets:insert_new(Tab,{{msync_userid_max,value},    ?msync_userid_max}),
            ets:insert_new(Tab,{{msync_global_number,value}, ?msync_global_number}),
            ets:insert_new(Tab,{{msync_appkey,value},      ?msync_appkey}),
            ets:insert_new(Tab,{{msync_username,value},      ?msync_username}),
            ets:insert_new(Tab,{{msync_passwd,value},        ?msync_passwd}),
            ets:insert_new(Tab,{{msync_domain_name,value},   {domain,?msync_domain}}),
            ets:insert_new(Tab,{{msync_initialized,value},   true}),
            ts_timer:config(ts_config:get_default(Tab, msync_global_number));
        _Else ->
            ok
    end.
