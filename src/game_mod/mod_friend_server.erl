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

handle_call({accept , ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName , FromRoleid } , _From , State ) ->

  %%邀请成功，更新双方的好友列表


  ok;
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
    <<"list">> -> ok ;
    <<"bad_add">> -> ok ;
    <<"bad_del">> -> ok ;
    <<"bad_list">> -> ok
  end.

invite( ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName ) ->
  InviteRoleid = Data#m_friend_invite_tos.dest_roleid ,
  gen_server:call(?MODULE, {invite, Module , Method , ClientSock, Roleid, RoleName, InviteRoleid}).

accept( ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName ) ->
  FromRoleid = Data#m_friend_accept_tos.from_roleid ,
  gen_server:call(?MODULE , {accept , ClientSock, Module, Method, Data, _AccountName, Roleid, RoleName , FromRoleid }).


