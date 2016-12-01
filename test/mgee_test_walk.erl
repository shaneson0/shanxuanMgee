%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-14
%%% @doc simulate stress test, simulate one person walk on the map.
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_test_walk).


%%
%% Include files
%%
-include("mgee.hrl").
-include("game_pb.hrl").

%%
%% Exported Functions
%%
-export([
		 start/1,
		 stop/1
		]).

-record(testdata, {n, config, account, pwd, rolename, roleid, sex, skinid, vwid, x, y}).
%%
%% API Functions
%%
start( {N, Config} ) ->
	Pid = spawn(fun() -> start_test_proc(N, Config) end ),
	Pid.

stop( Pid ) ->
	% TODO: kill process id.
	Pid ! {kill, self()},
	ok.

%%
%% Local Functions
%%

start_test_proc(N, Config) ->
	%{A1,A2,A3} = now(), random:seed(A1, A2, A3 * N),
	%每次都用同一个初始化种子，可以保证测试结果，每次都是相同的。
	random:seed(N, N, N),
	
	{CHost, CPort, CTimeout, _CSpeed} = Config,
	
	AccountID = N + 1000,
	%帐号名是 “编号1001” “编号1002” ....
	AccountName = [16#E7, 16#BC, 16#96, 16#E5, 16#8F, 16#B7] ++ integer_to_list(AccountID),
	
	
	%角色名是“角色1001” “角色1002” ....
	RoleName = [16#E8, 16#A7, 16#92, 16#E8, 16#89, 16#B2] ++ integer_to_list(AccountID),
	Sex = random(1,2) - 1,
	Skin = random(1, 6),
	Pwd = integer_to_list(random(100000,999999)),
	
	State = #testdata{n = N, config = Config,
					  account = AccountName, pwd = Pwd,
					  rolename =  RoleName, roleid = 0, sex = Sex, skinid = Skin,
					  vwid = 10001, x = 100, y = 100
					  },
	
	?DEBUG("test module walk init: ~p", [State]),
	%%UserAttrObj = {obj, [ {userid, Userid}, {username, Username}, {sex, Sex}, {skin, Skin}, {x,0}, {y,0} ]},
	
	case gen_tcp:connect(CHost, CPort,
						[binary, {packet, 2}, {active, false}], 
						CTimeout  ) of
		{ok, LSock} -> 
			do_send_handshake(LSock),
			%%spawn(fun() -> recv_msg_loop(N, LSock, State) end ),
			test_loop(N, LSock, State);
		{error, Reason} -> 
			?DEBUG("~p start_test_proc ~p fail, reason: ~p~n", [self(), N, Reason ]),
			exit( {error, N, Reason})
	end.

%%=====================================================
%%开一个进程，循环接受服务器发过来的消息，并显示
recv_msg_loop(N, LSock, State) ->
  Req = do_mgee_recv(LSock),
  %%Data = amf3:decode(zlib:uncompress(Req)),  
  ?DEBUG("~p ~p ~p receive msg: ~p~n", [self(), LSock, N, Req]),

  recv_msg_loop(N, LSock, State).


%%=====================================================
st_login(N, LSock, State) ->
	%开始登录帐号
	AccountObj = #m_login_flash_login_tos{ account_name = State#testdata.account,
										account_pwd = State#testdata.pwd},
%	AccountObj = #m_login_flash_login_tos{ account_name = "abc",
%										account_pwd =  "111111" },
	BinSend = game_pb:encode_m_login_flash_login_tos(AccountObj),
	do_mgee_send(N, LSock, <<"login">>, <<"flash_login">>, BinSend ),
	{<<"login">>, <<"flash_login">>, Req} = do_mgee_recv(LSock),	
	
	Data = game_pb:decode_m_login_flash_login_toc(Req),
	?DEBUG("[~p] receive msg: ~p~n", [N, Data]),
	{Reply , RoleId} = case Data#m_login_flash_login_toc.succ of 
		false ->
			?INFO_MSG("[~p] login fail: ~p", [N, Data#m_login_flash_login_toc.reason]),
			{error, 0};
		true ->
			%登录帐号成功
			List1 = Data#m_login_flash_login_toc.result,
			RoleList = case List1#p_role_list.role of 
				undefined ->
					[];
				A when is_list(A) ->
					A;
				_ ->
					[]
				end,
			?INFO_MSG("[~p] login succ, rolelist: ~p", [N, RoleList]),
			RID = case RoleList of
				[] ->	%创建一个新角色，并选中这个角色
						RoleListNew = st_create_role(N, LSock, State),
						case RoleListNew of
							{error, R} ->
								?DEBUG("[~p] st_create_role error: ~p", [N, R]),
								{error, R};
							_ ->
								st_select_role(N, LSock, State, RoleListNew)
						end;
				_  -> 	st_select_role(N, LSock, State, RoleList)
			end,
			if is_integer(RID) ->
				{ok , RID};
			true ->
				{error, 0}
			end;		
		R ->
			?DEBUG("[~p] recv login result error: ~p", [N, R]),
			{error, 0}
	end,
	%返回的状态中，包含有 选中的 RoleId
	{ Reply, State#testdata{roleid=RoleId} }.


% 创建一个新角色
st_create_role(N, LSock, State) ->
	AddRoleObj = #m_role_add_tos{ rolename = State#testdata.rolename,
								  sex = State#testdata.sex,
								  skinid = State#testdata.skinid
								  },
	BinSend = game_pb:encode_m_role_add_tos(AddRoleObj),
	do_mgee_send(N, LSock, <<"role">>, <<"add">>, BinSend ),
	%%Req = do_recv(LSock),
	{<<"role">>, <<"add">>, Req} = do_mgee_recv(LSock),
	Data = game_pb:decode_m_role_add_toc(Req),
	case Data#m_role_add_toc.succ of 
		false ->
			?INFO_MSG("[~p] create_role fail: ~p", [N, Data#m_role_add_toc.reason]),
			{ error, add_role_fail};		
		true -> 
			List1 = Data#m_role_add_toc.result,
			RoleList = List1#p_role_list.role,
			?INFO_MSG("[~p] create_role succ: ~p", [N, Data]),
			RoleList;
		R ->
			?DEBUG("[~p] recv create_role result error: ~p", [N, R]),
			{ error, add_role_fail}
	end.


% 选择一个已经存在的角色，返回 RoleID
st_select_role(_N, _LSock, State, RoleList) ->
	FoundRoleName = State#testdata.rolename,
	?DEBUG("st_select_role: ~p",[RoleList]),
	[Role] = lists:filter( fun(T) -> 
			T#p_game_role.rolename =:= FoundRoleName
				end, RoleList),
	?DEBUG("st_select_role result: ~p",[Role]),
	Role#p_game_role.roleid.
	

%开始使用指定角色ID，进入游戏
st_enter_game(N, LSock, State) ->
	ObjEnter = #m_role_enter_tos{ roleid = State#testdata.roleid },
	BinSend = game_pb:encode_m_role_enter_tos(ObjEnter),
	do_mgee_send(N, LSock, <<"role">>, <<"enter">>, BinSend ),
	{<<"role">>, <<"enter">>, Req} = do_mgee_recv(LSock),
	Data = game_pb:decode_m_role_enter_toc(Req),
	?DEBUG("[~p] receive msg: ~p~n", [N, Data]),
	Reply = case Data#m_role_enter_toc.succ of 
		false ->
			?INFO_MSG("[~p] role_enter fail: ~p", [N, Data#m_role_enter_toc.reason]),
			error;
		true ->
			_RoleAttr = Data#m_role_enter_toc.result,
			ok;
		R ->
			?DEBUG("[~p] recv role_enter result error: ~p", [N, R]),
			error
	end,
	{ Reply, State}.

	
test_vm_enter(N, LSock, Vmid) ->
	ObjEnter = #m_vw_enter_tos{ vwid = Vmid },
	BinSend = game_pb:encode_m_vw_enter_tos(ObjEnter),
	do_mgee_send(N, LSock, <<"vw">>, <<"enter">>, BinSend ),

	{<<"vw">>, <<"enter">>, Req} = do_mgee_recv(LSock),
	Data = game_pb:decode_m_vw_enter_toc(Req),
	case Data#m_vw_enter_toc.succ of 
		false ->
			%?INFO_MSG("[~p] vw enter fail: ~p", [N, Data#m_vw_enter_toc.reason]),
			{ error, vw_enter_fail};
		true ->
			%%% TODO: 未完成
			?INFO_MSG("[~p] vw enter ~p succ.", [N, Vmid]),
			ok;
		R ->
			?DEBUG("[~p] recv vw_enter result error: ~p", [N, R]),
			{ error, vw_enter_fail}
	end.
	

%% 开始模拟了，先是发登录消息
test_loop(N, LSock, State) ->
	%如果返回的结果，不是 ok, ... 则进程立即出错结束掉。
	{ok , State2} = st_login(N, LSock, State),

	%使用指定角色ID，进入游戏内
	{ok , State3} = st_enter_game(N, LSock, State2),
	
	test_vm_enter(N, LSock, State3#testdata.vwid),

	%开个进程去取数据，取完数据后不理它，已经不需要理会了。
	spawn(fun() -> recv_msg_loop(N, LSock, State) end ),

	Px = random(1,30),	%%初始化站在哪个点
	Py = random(1,16),	
	loop_walk(N, LSock, 0, Px, Py).

	
%%自动间隔一段时间，就向服务器发送一条消息
loop_walk(N, LSock, M, X, Y) ->
	{_DATE, TIME} = calendar:now_to_local_time(now()),
	Tx = random(1,30),
	Ty = random(1,16),
	
	%由(X,Y) 走到(Px,Py)需要走多少毫秒
	T = calc_move_time(Tx, Ty, X, Y),
	%%io:format("~p sleep ~p ms~n", [N, T] ),

	?DEBUG("Time:~p  ~p (~p,~p) goto (~p,~p), ~p ms~n", [ TIME, N, X, Y, Tx, Ty, T] ),
	
	Data = #m_move_walk_tos{dir=1, px=Tx * 40, py=Ty * 40, tx=Tx, ty=Ty },
	BinData = game_pb:encode_m_move_walk_tos(Data),
	do_mgee_send(N, LSock, <<"move">>, <<"walk">>, BinData ),
	
	sleep(T),
	loop_walk(N, LSock, M+1, Tx, Ty).

%%=====================================================

do_mgee_send(N, LSock, Mname, Fname, Data ) ->
	ZlibData = zlib:compress( Data ),	
	Mlen = length(binary_to_list(Mname)),
	Flen = length(binary_to_list(Fname)),	
	LL = << Mlen : 8 , Flen : 8 ,  Mname/binary, Fname/binary,  ZlibData/binary >>,
	{_DATE, TIME} = calendar:now_to_local_time(now()),
	?DEBUG("Time:~p  ~p call [~p:~p] ~p~n", [ TIME, N, Mname, Fname, ZlibData] ),
	do_send(LSock, LL).


do_mgee_recv(LSock) ->
	DataRaw = do_recv(LSock),
	<<ModuleLen:8, MethodLen:8, Bin/binary>> = DataRaw,
	{Module, Bin2} = split_binary(Bin, ModuleLen),
	{Method, Data} = split_binary(Bin2, MethodLen),
	{Module, Method, zlib:uncompress(Data) }.


% 发送一个 23个字节的包
do_send_handshake(LSock) ->		
	% 因为连接使用了 {packet, 2} 参数的原因，所以这里只需要发送21个字节的数据，会自动被加上2个字节的头部的
	Msg = list_to_binary(lists:seq(1,21,1)),
	do_send(LSock,Msg).

%% send a line of text to the socket
do_send(LSock,Msg) ->
  case gen_tcp:send(LSock, Msg) of
    ok -> ok;
    {_, Reason} -> 
		?ERROR_MSG("~p ~p send msg fail. ~p~n~p~n", [self(), LSock, Msg, Reason] ),
		exit(Reason)
  end.

%% receive data from the socket
do_recv(LSock) ->
  case gen_tcp:recv(LSock, 0) of
    {ok, Bin} -> Bin;
    {_, Reason} -> 
		?ERROR_MSG("~p ~p receive socket fail.~p~n", [self(), LSock, Reason] ),
		exit(Reason)
  end.


%%=====================================================
%% 暂停 毫秒
sleep(Msec) ->
	receive
		after Msec ->
			true
	end.

%% 随机数
random(Min,Max)->
	Min2 = Min-1,
	random:uniform(Max-Min2)+Min2.


calc_distance(X1,Y1,X2,Y2) ->
	math:sqrt( (X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2) ).

%% 计算从(x1,y1)走到(x2,y2)所需要的时间（豪秒）
calc_move_time(X1,Y1,X2,Y2) ->
	round( calc_distance(X1,Y1,X2,Y2) / 2 * 1000 ).


