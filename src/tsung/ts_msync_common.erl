%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
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

-module(ts_msync_common).
-vc('$Id$ ').
-author('left2right@easemob.com').

-export([ get_message/1,
          starttls/0
         ]).

-include("ts_macros.hrl").
-include("ts_profile.hrl").
-include("ts_msync.hrl").
-include("pb_msync.hrl").


%%----------------------------------------------------------------------
%% Func: get_message/1
%% Args: #msync record
%% Returns: binary
%% Purpose: Build a message/request from a #msync record
%%----------------------------------------------------------------------
get_message(Msync=#msync{regexp=RegExp}) when RegExp /= undefined->
    put(regexp, RegExp),
    get_message(Msync#msync{regexp=undefined});
get_message(_Msync=#msync{type = 'wait'}) ->
    << >>;
get_message(Msync=#msync{id=user_defined,appkey=Appkey,username=Username,passwd=Pwd,domain=Domain,resource=Resource,type = 'connect'}) ->
   JID = #'JID'{
             app_key = list_to_binary(Appkey),
             name = list_to_binary(Username),
             domain = list_to_binary(Domain),
             client_resource = list_to_binary(Resource)
            },
    ts_user_server:add_to_connected({JID,Pwd}),
    ts_user_server:add_to_online(set_id(user_defined,JID,Pwd)),
    connect(Msync);
get_message(Msync=#msync{type = 'connect'}) ->
    connect(Msync);
get_message(#msync{type = 'starttls'}) ->
    starttls();
get_message(#msync{type = 'close', id=Id,username=User,passwd=Pwd,user_server=UserServer}) ->
    ts_user_server:remove_connected(UserServer,set_id(user_defined,User,Pwd)),
    close();

get_message(Msync=#msync{type = 'chat', id=Id, dest=online,appkey=Appkey,username=User,passwd=Pwd, prefix=Prefix,
                           domain=Domain,resource=Resource,user_server=UserServer})->
   JID = #'JID'{
             app_key = list_to_binary(Appkey),
             name = list_to_binary(User),
             domain = list_to_binary(Domain),
             client_resource = list_to_binary(Resource)
            },
    case ts_user_server:get_online(UserServer,set_id(user_defined,JID,Pwd)) of
        {ok, {Dest,_}} ->
            message(JID, Dest, Msync, Domain);
        {ok, Dest} ->
            message(JID, ts_msync:username(Prefix,Dest), Msync, Domain);
        {error, no_online} ->
            ts_mon:add({ count, error_no_online }),
            << >>
    end;

get_message(Msync=#msync{type = 'chat',domain=Domain,prefix=Prefix,dest=offline,user_server=UserServer})->
    case ts_user_server:get_offline(UserServer) of
        {ok, {Dest,_}} ->
            message(Dest, Dest, Msync, Domain);
        {ok, Dest} ->
            message(ts_msync:username(Prefix,Dest), ts_msync:username(Prefix,Dest), Msync, Domain);
        {error, no_offline} ->
            ts_mon:add({ count, error_no_offline }),
            << >>
    end;
get_message(Msync=#msync{type = 'chat', dest=random, prefix=Prefix, domain=Domain,user_server=UserServer}) ->
    case ts_user_server:get_id(UserServer) of
        {error, Msg} ->
            ?LOGF("Can't find a random user (~p)~n", [Msg],?ERR),
            << >>;
        {Dest,_} ->
            message(Dest, Dest, Msync, Domain);
        DestId    ->
            message(ts_msync:username(Prefix,DestId), ts_msync:username(Prefix,DestId), Msync, Domain)
    end;

get_message(Msync=#msync{type = 'chat', dest=unique, prefix=Prefix, domain=Domain,user_server=UserServer})->
    case ts_user_server:get_first(UserServer) of
        {Dest, _}  ->
            message(Dest, Dest, Msync, Domain);
        IdDest ->
            message(ts_msync:username(Prefix,IdDest), ts_msync:username(Prefix,IdDest), Msync, Domain)
    end;
get_message(_Msync=#msync{type = 'chat', id=_Id, dest = undefined, domain=_Domain}) ->
    %% this can happen if previous is set but undefined, skip
    ts_mon:add({ count, error_no_previous }),
    << >>;
get_message(Msync=#msync{type = 'chat', id=_Id, dest = Dest, domain=Domain}) ->
    ?DebugF("~w -> ~w ~n", [_Id,  Dest]),
    message(Dest, Dest, Msync, Domain);

%% MUC benchmark support
get_message(#msync{type = 'muc:chat', appkey=Appkey, room = Room, muc_service = Service, username=User, size = Size, domain=Domain, resource=Resource}) ->
    JID = #'JID'{
             app_key = list_to_binary(Appkey),
             name = list_to_binary(User),
             domain = list_to_binary(Domain),
             client_resource = list_to_binary(Resource)
            },
    muc_chat(Appkey, Room, Service, Size, JID).

%%----------------------------------------------------------------------
%% Func: make_JID/4
%%----------------------------------------------------------------------
make_JID(Appkey,Username,Domain,Resource) ->
    #'JID'{
             app_key = Appkey,
             name = Username,
             domain = Domain,
             client_resource = Resource
            }.

%%----------------------------------------------------------------------
%% Func: connect/1
%%----------------------------------------------------------------------
connect(#msync{appkey=Appkey,username=Username,passwd=Password,domain=Domain,resource=Resource}) ->
    JID = make_JID(list_to_binary(Appkey),list_to_binary(Username),list_to_binary(Domain),list_to_binary(Resource)),
    MSync = #'MSync'{
               guid = JID,
               auth = list_to_binary(Password),
               command = 'PROVISION',
               compress_algorimth = undefined,
               payload = #'Provision'{compress_type = ['COMPRESS_NONE']}
              },
    msync_msg:encode(MSync, undefined).

%%----------------------------------------------------------------------
%% Func: close/0
%% Purpose: close msync session
%%----------------------------------------------------------------------
close () -> list_to_binary("</stream:stream>").

%%----------------------------------------------------------------------
%% Func: starttls/0
%% Purpose: send the starttls element
%%----------------------------------------------------------------------
starttls()->
    <<"<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>">>.

%%----------------------------------------------------------------------
%% Func: message/3
%% Purpose: send message to defined user at the Service (aim, ...)
%%----------------------------------------------------------------------
message(From, Dest, #msync{size=Size,data=undefined},
        _Service) when is_integer(Size) ->
    generate_stamp(false),
    Text = list_to_binary(ts_utils:urandomstr_noflat(Size)),
    put(previous, Dest),
    MetaPayload =
        chain:apply(
          msync_msg_ns_chat:new(),
          [
           {msync_msg_ns_chat, chat, [Text]},
           {msync_msg_ns_chat, from, [From#'JID'.name]},
           {msync_msg_ns_chat, to, [Dest#'JID'.name]}]),
    Meta = #'Meta'{
              id = erlang:abs(erlang:unique_integer()),
              from = From,
              to = Dest,
              ns = 'CHAT',
              payload = MetaPayload
             },
    Payload = #'CommSyncUL'{ meta = Meta},
    MSync = #'MSync'{
               command = 'SYNC',
               compress_algorimth = undefined,
               payload = Payload
              },
    msync_msg:encode(MSync, undefined);


message(From, Dest, #msync{data=Data}, _Service) when is_list(Data) ->
    Text =  list_to_binary(Data),
    put(previous, Dest),
    MetaPayload =
        chain:apply(
          msync_msg_ns_chat:new(),
          [
           {msync_msg_ns_chat, chat, [Text]},
           {msync_msg_ns_chat, from, [From#'JID'.name]},
           {msync_msg_ns_chat, to, [Dest#'JID'.name]}]),
    Meta = #'Meta'{
              id = erlang:abs(erlang:unique_integer()),
              from = From,
              to = Dest,
              ns = 'CHAT',
              payload = MetaPayload
             },
    Payload = #'CommSyncUL'{ meta = Meta},
    MSync = #'MSync'{
               command = 'SYNC',
               compress_algorimth = undefined,
               payload = Payload
              },
    msync_msg:encode(MSync, undefined).


generate_stamp(false) ->
    "";
generate_stamp(true) ->
    {Mega, Secs, Micro} = erlang:now(),
    TS = integer_to_list(Mega) ++ ";"
    ++ integer_to_list(Secs) ++ ";"
    ++ integer_to_list(Micro),
    "@@@" ++ integer_to_list(erlang:phash2(node())) ++ "," ++ TS ++ "@@@".

%%message(Dest, #msync{data=Data,appkey=Appkey,username=User,passwd=Pwd,resource=Resource}, Service) when is_list(Data) ->
muc_chat(Appkey, Room, Service, Size, From) ->
    Text =  list_to_binary(ts_utils:urandomstr_noflat(Size)),
    ToJID = make_JID(list_to_binary(Appkey),list_to_binary(Room),list_to_binary(Service),undefined),
    MetaPayload =
        chain:apply(
          msync_msg_ns_chat:new(),
          [
           {msync_msg_ns_chat, gchat, [Text]},
           {msync_msg_ns_chat, from, [From#'JID'.name]},
           {msync_msg_ns_chat, to, [Room]}]),
    Meta = #'Meta'{
              id = erlang:abs(erlang:unique_integer()),
              from = From,
              to = ToJID,
              ns = 'CHAT',
              payload = MetaPayload
             },
    Payload = #'CommSyncUL'{ meta = Meta},
    MSync = #'MSync'{
               command = 'SYNC',
               compress_algorimth = undefined,
               payload = Payload
              },
    msync_msg:encode(MSync, undefined).

%% set the real Id; by default use the Id; but it user and passwd is
%% defined statically (using csv for example), Id is the tuple { User, Passwd }
set_id(user_defined,User,Passwd) ->
    {User,Passwd};
set_id(Id,_User,_Passwd) ->
    Id.
