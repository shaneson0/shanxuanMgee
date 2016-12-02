%%%-------------------------------------------------------------------
%%% @author chenshanxuan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 十一月 2016 17:56
%%%-------------------------------------------------------------------
-module(mod_friend_server).
-author("chenshanxuan").

-behaviour(gen_server).

-include("mgee_vo.hrl").
-include("mgee.hrl").
-include("game_pb.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([start/0,handle/1]).

-export([create_relation/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
  {ok, _} = supervisor:start_child(
    mgee_sup,
    {mod_friend_server,
      {mod_friend_server, start_link, []},
      transient, brutal_kill, worker, [mod_friend_server]}),
  ok.

create_relation(Role_id) ->
  ?DEBUG("create relation : ~p ~n ", [Role_id]) ,
  gen_server:cast( ?MODULE , {create , Role_id} ).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->

  ?INFO_MSG(" CREATE ETS_REATION  ~p~n" , ["aa"] ),
  ets:new( ?ETS_RELATION , [set , protected , named_table ] ),

  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).



handle_call( {enemy_add,ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName,Enemy_id} , _From , State ) ->
  Reply = insert_badlist_or_enemylist(Roleid , Enemy_id , enemy ),
  {reply , Reply , State } ;
handle_call({bad_add,ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName,Add_Role_id},_From , State ) ->
  Reply = insert_badlist_or_enemylist(Roleid , Add_Role_id , badlist ),
  {reply , Reply , State};
handle_call({bad_list , ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName},_From , State ) ->
  Reply = get_role_list(Roleid,blacklist),
  {reply , Reply , State };
handle_call({enemy_list , ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName},_From , State ) ->
  Reply = get_role_list(Roleid,enemy),
  {reply , Reply , State };
handle_call({list, _ClientSock, _Module, _Method, _Data, _AccountName, Roleid, _RoleName } , _From , State ) ->
  Reply = get_role_list(Roleid,friend),
  {reply,Reply,State};
handle_call({accept , _ClientSock, _Module, _Method, Data, _AccountName, Roleid, _RoleName , FromRoleid } , _From , State ) ->

  %%邀请成功，更新双方的好友列表
  insert_friend_ets(Roleid,FromRoleid),

  case mgee_misc:get_socket_by_roleid(FromRoleid) of
    {ok , ToClientSocket} ->
      DataRecord = #m_friend_accept_toc{succ = true , return_self = false },
      mgee_packet:packet_encode_send( ToClientSocket , _Module , _Method , DataRecord );
    true ->
      %%被邀请人用户不在线并没有关系
      ok
  end,

  Reply = #m_friend_accept_toc{dest_roleid = FromRoleid},
  {reply,Reply,State};
handle_call( {invite, Module , Method ,  ClientSock, Roleid, RoleName, InviteRoleid} , _From , State ) ->
  ?DEBUG("~p invite good friend , roleid:~p,",[self(), Roleid]),

  %todo ,返回给InviteRoleid相应的信息

  case mgee_misc:get_socket_by_roleid(InviteRoleid) of
    {ok, ToClientSock} ->
      DataRecord = #m_friend_invite_toc{ succ = true , return_self = false , from_roleid = Roleid , from_rolename = RoleName  } ,
      mgee_packet:packet_encode_send(ToClientSock, Module, Method, DataRecord);
    Wrong ->
      DataRecord = #m_chat_private_toc{succ=false, reason= <<"用户不在线！">>},
      mgee_packet:packet_encode_send(ClientSock, Module, Method, DataRecord),
      ?DEBUG("find pid ~p socket failed", Wrong)
  end,


  %%返回给用户表示邀请成功
  Reply = #m_friend_invite_toc{succ = true , return_self = true },
  {reply , Reply , State } ;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({create , Role_id} , State ) ->
  ets:insert( ?ETS_RELATION , {Role_id ,  #relation{ friends = [] , blacklist = [] , enemy = [] , offended_list = [] } } ),
  {noreply , State };
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


handle({ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName}) ->
  ?DEBUG("~p ~p ~p", [Module, Method, Data]),
  case Method of

    <<"invite">> ->
      invite(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName);
    <<"accept">> ->
      accept( ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName );
    <<"list">> ->
      list(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName);
    <<"bad_add">> ->
      bad_add(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName);
    <<"bad_list">> ->
      bad_list(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName);
    <<"enemy_add">> ->
      enemy_add(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName);
    <<"enemy_list">> ->
      enemy_list(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName)
  end.

bad_add(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName) ->
  Add_Role_id = Data#m_friend_bad_add_tos.roleid ,
  gen_server:call(?MODULE , {bad_add,ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName,Add_Role_id}).

bad_list(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName) ->
  gen_server:call( ?MODULE , {bad_list , ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName }).

enemy_add(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName) ->
  Enemy_id = Data#m_friend_enemy_add_tos.roleid ,
  gen_server:call(?MODULE , {enemy_add,ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName,Enemy_id}).

enemy_list(ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName) ->
  gen_server:call(?MODULE , {enemy_list , ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName}).


invite( ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName ) ->
  InviteRoleid = Data#m_friend_invite_tos.dest_roleid ,
  gen_server:call(?MODULE, {invite, Module , Method , ClientSock, Roleid, RoleName, InviteRoleid}).

accept( ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName ) ->
  FromRoleid = Data#m_friend_accept_tos.from_roleid ,
  gen_server:call(?MODULE , {accept , ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName , FromRoleid }).

list( ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName ) ->
  gen_server:call(?MODULE , {list, ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName } , infinity ) .

insert_friend_ets(From_id , Dest_id) ->
  %Dest to From
  [{ _ , OldRelation } ] = ets:lookup( ?ETS_RELATION , From_id ),
  OldFriendList = OldRelation#relation.friends ,
  NewFriendList = [ Dest_id | OldFriendList ] ,
  ets:insert( ?ETS_RELATION ,  { From_id , OldRelation#relation{friends = NewFriendList}  } ) ,

  %From to Dest
  [{ _ , OldRelation1 } ] = ets:lookup( ?ETS_RELATION , Dest_id ),
  OldFriendList1 = OldRelation1#relation.friends ,
  NewFriendList1 = [ From_id | OldFriendList1 ] ,
  ets:insert( ?ETS_RELATION , { Dest_id ,  OldRelation#relation{friends = NewFriendList1} } ).


get_role_list(Roleid , Options ) ->
  [{ _ , Relation_record}] = ets:lookup(?ETS_RELATION , Roleid ) ,
  case Options of
    friend ->
      #m_friend_list_toc{friendlist = Relation_record#relation.friends } ;
    enemy ->
      #m_friend_enemy_list_toc{enemylist = Relation_record#relation.enemy } ;
    badlist ->
      #m_friend_bad_list_toc{ blacklist  = Relation_record#relation.blacklist }
  end.

insert_badlist_or_enemylist( From_Role_id , Dest_Role_id , Options ) ->
  [{ _ , OldRelation } ] = ets:lookup( ?ETS_RELATION , From_Role_id ),
  case Options of
     enemy ->
       OldEnemyList = OldRelation#relation.enemy ,
       NewEnemyList = [ Dest_Role_id | OldEnemyList ] ,
       ets:insert( ?ETS_RELATION ,  { From_Role_id , OldRelation#relation{enemy   = NewEnemyList}  } ),
       #m_friend_enemy_add_toc{succ = true};
    badlist ->
      OldBadList = OldRelation#relation.blacklist ,
      NewBadList = [ Dest_Role_id | OldBadList ] ,
      ets:insert( ?ETS_RELATION ,  { From_Role_id , OldRelation#relation{blacklist    = NewBadList}  } ),
      #m_friend_bad_add_toc{succ = true}
end.







