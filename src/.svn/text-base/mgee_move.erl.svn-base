%%%----------------------------------------------------------------------
%%% File    : mgee_walk.erl
%%% Author  : Qingliang
%%% Created : 2010-1-6
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------
-module(mgee_move).

%%
%% Include files
%%
-include("mgee.hrl").
-include("game_pb.hrl").
%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%

handle({ClientSock, Module, Method, DataRecord, AccountName, RoleId, RoleName}) ->
	case Method of
		<<"walk">> -> walk(ClientSock, Module, Method, DataRecord, RoleId);
		<<"walk_path">> -> walk_path(RoleId, Method, DataRecord);
		<<"run">> -> run(RoleId, DataRecord);
		<<"fly">> -> fly(RoleId, DataRecord);
		<<"sit">> -> sit(RoleId, DataRecord);
		<<"follow">> -> follow(RoleId, DataRecord)
	end,
	ok.

%%
%% Local Functions
%%

walk(ClientSock, Module, Method, DataRecord, RoleId) ->
	case mgee_virtual_world_router:get_role_state(RoleId) of
		{ok, RoleState} ->
			VName = mgee_virtual_world_router:get_virtual_world_name(RoleState#p_game_role.vwid),
			gen_server:cast(VName, {walk, ClientSock, Module, Method, DataRecord, RoleId});
		{error, not_found} ->
			?INFO_MSG("role ~p not in vw", [RoleId])
	end.

walk_path(RoleId, Method, DataRecord) ->
	ok.


%% data is like this {id:123,x:12,y:34}
run(From, Data) ->
	%%Data2 = zlib:compress(amf3:encode(Data)),
	AllClient = pg2:get_members(pg2_all_client),
	Clients = lists:delete(From, AllClient),
	DataSend = mgee_packet:encode(<<"move">>, <<"run">>, Data),
	lists:foreach(fun(Pid) ->
				  	Pid ! {run, DataSend}
				end,
				Clients),
	ok.

%% data is like this {id:123,x:12,y:34}
fly(From, Data) ->
	%%Data2 = zlib:compress(amf3:encode(Data)),
	AllClient = pg2:get_members(pg2_all_client),
	Clients = lists:delete(From, AllClient),
	DataSend = mgee_packet:encode(<<"move">>, <<"fly">>, Data),
	lists:foreach(fun(Pid) ->
				  	Pid ! {fly, DataSend}
				end,
				Clients),
	ok.

%% data is like this {id:123,x:12,y:34}
sit(From, Data) ->
	%%Data2 = zlib:compress(amf3:encode(Data)),
	AllClient = pg2:get_members(pg2_all_client),
	Clients = lists:delete(From, AllClient),
	DataSend = mgee_packet:encode(<<"move">>, <<"sit">>, Data),
	lists:foreach(fun(Pid) ->
				  	Pid ! {sit, DataSend}
				end,
				Clients),
	ok.

%% data is like this {id:123,x:12,y:34}
%% follow the team leader's walk.
follow(From, Data) ->
	%%Data2 = zlib:compress(amf3:encode(Data)),
	AllClient = pg2:get_members(pg2_all_client),
	Clients = lists:delete(From, AllClient),
	DataSend = mgee_packet:encode(<<"move">>, <<"follow">>, Data),
	lists:foreach(fun(Pid) ->
				  	Pid ! {follow, DataSend}
				end,
				Clients),
	ok.

