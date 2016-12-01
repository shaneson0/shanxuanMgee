%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-19
%%% @doc every team use one mod_team gen_server to provide role data and team data,
%%%		when the first person join the team, create one mod_team gen_server.
%%% @end
%%%----------------------------------------------------------------------

-module(mod_team).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("mgee.hrl").
-include("game_pb.hrl").
-include("global_lang.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record( mod_team_status, {
					 teamid,
					 teamdesc,
					 create_time, 
					 team_role_list , 
					 pid_list, 
					 list_invite,
					 list_request,
					 list_bad_invite,
					 list_bad_request} ).

%% ====================================================================
%% External functions
%% ====================================================================


start_link({Teamid, TeamDesc, Roleid, RoleName}) ->
	gen_server:start_link(?MODULE, {Teamid, TeamDesc, Roleid, RoleName}, []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init({Teamid, TeamDesc, Roleid, RoleName}) ->
	EtsInvite = ets:new(ets_invite, [set, private]), 
	EtsRequest = ets:new(ets_request, [set, private]),
	EtsPidList = ets:new(ets_pid_list, [set, private]),
	ets:insert(EtsPidList, {Roleid, mgee_misc:get_role_pid(Roleid) }),
	
	EtsTeamRoleList = ets:new(ets_team_role_list, [set, private]),
	ets:insert(EtsTeamRoleList, {Roleid,	#p_team_role{roleid= Roleid, 
														rolename=RoleName, 
														is_leader=true,
														team_order=1 
														}
								 }),
%% 	List = [#p_team_role{roleid= Roleid, 
%% 						 rolename=RoleName, 
%% 						 is_leader=true,
%% 						 team_order=1 
%% 						} ],
	State = #mod_team_status{teamid = Teamid, 
							 teamdesc = TeamDesc,
							 create_time = now(), 
							 team_role_list=EtsTeamRoleList,
							 pid_list = EtsPidList,
							 list_invite = EtsInvite,
							 list_request = EtsRequest
							},
    {ok,  State}.

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



% @doc handle list role event call
handle_call( {list, ClientSock, Roleid, RoleName}, _From, State) ->
	?DEBUG("~p list, roleid:~p,",[self(), Roleid]),
	Reply = State#mod_team_status.team_role_list,
    {reply, Reply, State};

handle_call( {invite, ClientSock, Roleid, RoleName, InviteRoleid}, _From, State) ->
	?DEBUG("~p invite, roleid:~p,",[self(), Roleid]),
	Teamid = State#mod_team_status.teamid,
	EtsInvite = State#mod_team_status.list_invite,
	TimeNow = mgee_timer:now(),
	ets:insert(EtsInvite, {InviteRoleid, TimeNow}),
	%% todo, send a msg to InviteRoleid
	
	Reply = #m_team_invite_toc{succ=true, teamid=Teamid},
    {reply, Reply, State};

% @doc accept and join in the team
handle_call( {accept, ClientSock, Roleid, RoleName, TeamId}, _From, State) ->
	%Teamid = State#mod_team_status.teamid,
	EtsTeamRoleList = State#mod_team_status.team_role_list,
	EtsInvite = State#mod_team_status.list_invite,
	EtsPidList = State#mod_team_status.pid_list,
	
	Reply = 
	case ets:lookup(EtsInvite, Roleid) of
		[{Roleid, _InviteTime}] -> 
			% found invite msg, then can accept and join.
			case ets:lookup(EtsTeamRoleList, Roleid) of
				[{Roleid, _TeamRole}] -> 
					#m_team_accept_toc{succ=false, reason=?_LANG_TEAM_ACCEPT_REPEAT};
				_ ->
					RolePid = mgee_misc:get_role_pid(Roleid),
					
					%% TODO: get same attr from role gen_server
					%%RoleAttr = gen_server:call(RolePid, {}),
					
					ets:insert(EtsPidList, {Roleid, RolePid }),
					ets:insert(EtsTeamRoleList, {Roleid, #p_team_role{
														roleid = Roleid, 
														rolename = RoleName, 
														%level = RoleAttr#..........
														team_order = get_next_team_order_num(EtsTeamRoleList),
														is_leader = false
														}
												}),
					ets:delete(EtsInvite, Roleid),
					#m_team_accept_toc{ list = ets:tab2list(EtsTeamRoleList),
										roleid = Roleid,
										rolename = RoleName,
										teamid = TeamId
									  }
			end;
		_ ->
			% not found invite msg
			#m_team_accept_toc{succ=false, reason=?_LANG_TEAM_ACCEPT_FAIL_NOT_INVITE}
	end,
	?DEBUG("accept team, ~p", [Reply]),
    {reply, Reply, State};

% @doc refuse the invite 
handle_call( {refuse, ClientSock, Roleid, RoleName, TeamId}, _From, State) ->
	%EtsTeamRoleList = State#mod_team_status.team_role_list,
	EtsInvite = State#mod_team_status.list_invite,
	%EtsPidList = State#mod_team_status.pid_list,
	
	Reply = 
	case ets:lookup(EtsInvite, Roleid) of
		[{Roleid, _InviteTime}] -> 
			ets:delete(EtsInvite, Roleid),
			#m_team_refuse_toc{
								roleid = Roleid,
								rolename = RoleName,
								teamid = TeamId
							  };
		_ ->
			ignore
	end,
	?DEBUG("refuse team, ~p", [Reply]),
    {reply, Reply, State};

% @doc leave/exit the team 
handle_call( {leave, ClientSock, Roleid, RoleName, TeamId}, _From, State) ->
	%Teamid = State#mod_team_status.teamid,
	EtsTeamRoleList = State#mod_team_status.team_role_list,
	EtsPidList = State#mod_team_status.pid_list,
	
	Reply = 
	case ets:lookup(EtsTeamRoleList, Roleid) of
		[{Roleid, TeamRole}] -> 
			RolePid = mgee_misc:get_role_pid(Roleid),
				
			case TeamRole#p_team_role.is_leader of 
				true ->
					% TODO: team leader leave, must change leader position to next person.
					% 
					ok;
				
				false ->
					ets:delete(EtsTeamRoleList, Roleid)
			end,			
			ets:delete(EtsPidList, Roleid ),
			
			#m_team_leave_toc{  list = ets:tab2list(EtsTeamRoleList),
								roleid = Roleid,
								rolename = RoleName,
								teamid = TeamId
							  };
		_ ->
			#m_team_leave_toc{succ=false, reason=?_LANG_TEAM_LEAVE_FAIL_NOT_IN}
	end,
	?DEBUG("leave team, ~p", [Reply]),
    {reply, Reply, State};

% @doc team member offline...
handle_call( {offline, ClientSock, Roleid, RoleName}, _From, State) ->
	%% TODO:.....
	Reply = doing,
	?DEBUG("offline team, ~p", [Reply]),
    {reply, Reply, State};


% @doc the team leader change 
handle_call( {change_leader, ClientSock, Roleid, RoleName, TeamId, ToRoleid, ToRoleName}, _From, State) ->
	%Teamid = State#mod_team_status.teamid,
	EtsTeamRoleList = State#mod_team_status.team_role_list,
	EtsPidList = State#mod_team_status.pid_list,
	
	Result = 
	case ets:lookup(EtsTeamRoleList, Roleid) of
		[{Roleid, TeamRole}] -> 
			RolePid = mgee_misc:get_role_pid(Roleid),
				
			case TeamRole#p_team_role.is_leader of 
				true ->
					% TODO: team leader leave, must change leader position to next person.
					% 
					case find_next_team_leader(EtsTeamRoleList) of 
						{ok, TeamOrder2, TeamRole2} ->
							%% TODO: ..
							ok;
						_ ->
							ok
					end,
					
					
					#m_team_change_leader_toc{  list = ets:tab2list(EtsTeamRoleList),
								roleid = Roleid,
								rolename = RoleName,
								teamid = TeamId
							  };
				false ->
					{error, not_leader }
			end;
		_ ->
			{error, not_leader }
	end,
	Reply = case Result of
		{error, not_leader} ->
			#m_team_change_leader_toc{succ=false, reason=?_LANG_TEAM_LEADER_AUTHORITY};
		_ ->
			Result
	end,
	?DEBUG("change_leader team, ~p", [Reply]),
    {reply, Reply, State};



handle_call(Request, From, State) ->
	?INFO_MSG("~p handle_cal from ~p : ~p", [?MODULE, From, Request]),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% socket closed
handle_info({'EXIT', _Pid, closed}, State) ->
	?INFO_MSG("the client socket connnection has closed, we need to save some data!!!", []),
	mgee_virtual_world_router:quit_vw(self()),
	{stop, "the client socket connnection has closed", State};
%% server is shutdown
handle_info({'EXIT', _Pid, shutdown}, State) ->
	?INFO_MSG("do_client function has quited with reason shutdown", []),
	{stop, "do_client function has quited with reason shutdown", State};
%% receive data timeout
handle_info({'EXIT', _Pid, timeout}, State) ->
	?INFO_MSG("do_client function has quited with reason timeout", []),
	{stop, "do_client function has quited with reason timeout", State};
handle_info({'EXIT', _Pid, normal}, State) ->
	?INFO_MSG("the client auth process has quit with reason normal", []),
	{noreply, State};
%% any other reason caused quit
handle_info({'EXIT', _Pid, Reason}, State) ->
	?INFO_MSG("do_client function has quited with reason ~p", [Reason]),
	{stop, Reason, State};
handle_info({event, start_client}, State) ->
	ClientSock = State#account_state.client_sock,
	Pid = self(),
	%spawn_link(fun() -> do_client_spawn(Pid, ClientSock) end),
	{noreply, State};
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

get_next_team_order_num(EtsTeamRoleList) ->
	CurMax = lists:max(
				lists:map(
					fun(T) -> T#p_team_role.team_order
					end, ets:tab2list(EtsTeamRoleList)
					)
			),
	?DEBUG("get_next_team_order_num, cur_max: ~p", [CurMax]),
	(CurMax + 1).

% @doc find next person who can become team leader,
%  sort the list, and get the second order's data.
find_next_team_leader(EtsTeamRoleList) ->
	List = ets:tab2list(EtsTeamRoleList),
	if (length(List) > 1) ->
		SortList = lists:sort(
				lists:map(
					fun(T) -> {T#p_team_role.team_order, T}
					end, List
					)
			),
		{TeamOrder, TeamRole} = lists:nth( 2, SortList),
		{ok, TeamOrder, TeamRole};	
	true ->
		{error, limit}
	end.

