%%%----------------------------------------------------------------------
%%% File    : mgee_virtual_world.erl
%%% Author  : Qingliang
%%% Created : 2010-1-4
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------
-module(mgee_virtual_world).

-include("mgee.hrl").
-include("game_pb.hrl").

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(vw_state, {vmid, pg2_name}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

start_link({ServerName, VMId}) ->
	gen_server:start_link({local, ServerName}, ?MODULE, VMId, []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(VMId) ->
	%% use ets to save the people in this virtual world
	Name = list_to_atom(lists:flatten(io_lib:format("pg2_virtual_world_client_list_~w", [VMId]))),
	pg2:create(Name),
    {ok, #vw_state{vmid=VMId, pg2_name=Name}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({quit, Pid}, From, #vw_state{pg2_name=Pg2} = State) ->
	Reply =	case lists:member(Pid, pg2:get_members(Pg2)) of
		false -> From ! {error, not_in};
		%% not found , add it
		true ->  pg2:leave(Pg2, Pid),
	   		   From ! {ok, quited}
	end,
	{reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% chat in virtual world, just send a message to every client in the current vw
%% because we don't need to reply the caller ,so we use the handle_cast function
handle_cast({quit, Pid, Roleid}, #vw_state{pg2_name=Pg2} = State) ->
	?INFO_MSG("before leave ~p ~p", [Pid, Pg2]),
	Rtn = pg2:leave(Pg2, Pid),
	?INFO_MSG("after leave ~p", [Rtn]),
	Data = mgee_packet:encode(<<"vw">>, <<"quit">>, #m_vw_quit_toc{roleid=Roleid}),
	DataSend = mgee_packet:packet(<<"vw">>, <<"quit">>, Data),
	spawn(fun() -> broadcast(pg2:get_members(Pg2), DataSend) end),
	{noreply, State};
handle_cast({enter, Pid}, #vw_state{pg2_name=Pg2} = State) ->
	?INFO_MSG("response enter virtual ~p ", [Pid]),
	case lists:member(Pid, pg2:get_members(Pg2)) of
			true -> error;
			%% not found , add it
			false ->  pg2:join(Pg2, Pid)
	end,
	spawn(fun() -> enter(Pid, Pg2) end),
	{noreply, State};
handle_cast({walk, ClientSock, Module, Method, DataIn, Roleid}, State) ->
	%% check whether can walk
	Tx = DataIn#m_move_walk_tos.tx,
	Ty = DataIn#m_move_walk_tos.ty,
	Px = DataIn#m_move_walk_tos.px,
	Py = DataIn#m_move_walk_tos.py,
	gen_server:cast(mgee_virtual_world_router, {walk, Roleid, Tx, Ty, Px, Py}),
	%%DataRecord = #m_move_walk_toc{succ=false,tx=Tx,ty=Ty,px=Px,py=Py},
	DataRecord = #m_move_walk_toc{},
	mgee_packet:packet_encode_send(ClientSock, Module, Method, DataRecord),
	{noreply, State};
handle_cast({vw_chat, Content}, #vw_state{pg2_name=Pg2} = State) ->
	lists:foreach(fun(ClientPid) -> ClientPid ! {vm_chat, Content} end, pg2:get_members(Pg2)),
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
walk_path(PidList, Data) ->
	broadcast(PidList, Data).

broadcast(PidList, Data) ->
	lists:foreach(
		fun(Pid) -> 
			[{Pid, ClientSocket, _}] = ets:lookup(mgee_tcp_client_info_list, Pid),
			mgee_packet:send(ClientSocket, Data)
		end, PidList),
	ok.

enter(Pid, Pg2) ->
	case ets:lookup(mgee_tcp_client_info_list, Pid) of
		[{Pid, ClientSocket, RoleState}] -> 
			AllPid = lists:delete(Pid, pg2:get_members(Pg2)),
			?DEBUG("enter data send ~p", [AllPid]),
			AllClient = lists:foldl(
							fun(CPid, ACC0) ->
								[{CPid, _, ClientState2}] = ets:lookup(mgee_tcp_client_info_list, CPid),
								[ClientState2|ACC0]
							end, [], AllPid),
			?DEBUG("enter data send AllClient : ~p", [AllClient]),
%%			DataRecord = #m_vw_enter_toc{result = #p_role_list{role=AllClient}},
			DataRecord = #m_vw_enter_toc{ result = #p_role_status_list{role_status = [ #p_role_server_status{roleinfo = X} || X <- AllClient ]} } ,
			?DEBUG("enter data send2 ~p", [DataRecord]),
			mgee_packet:packet_encode_send(ClientSocket, <<"vw">>, <<"enter">>, DataRecord),
			DataRecord2 = #m_vw_enter_toc{
										  return_self=false, 
										  roleid=RoleState#p_game_role.roleid,
										  game_role = RoleState
										 },
			DataOther = mgee_packet:packet(
						  "vw", 
						  "enter", 
						  mgee_packet:encode("vw", "enter", DataRecord2)),
			broadcast(lists:delete(Pid, pg2:get_members(Pg2)), DataOther);
		_ -> 
			error
	end.