%%%----------------------------------------------------------------------
%%% File    : mgee_tcp_acceptor.erl
%%% Author  : Qingliang
%%% Created : 2010-01-02
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-module(mgee_tcp_acceptor).

-include("mgee.hrl").

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%-record(state, {callback, sock, ref}).

%%--------------------------------------------------------------------

start_link(Callback, LSock) ->
	gen_server:start_link(?MODULE, {Callback, LSock}, []).

%%--------------------------------------------------------------------

init({_Callback, LSock}) ->
	{ok, LSock}.

handle_info({event, start}, LSock = State) ->
	case gen_tcp:accept(LSock) of
		{ok, ClientSock} -> {ok, CPid} = supervisor:start_child(mgee_account_sup, [ClientSock]),
							PidAccountSup = mgee_misc:whereis_name({local, mgee_account_sup}),
							%% mgee_account_sup controlling all the clientSocket
							gen_tcp:controlling_process(ClientSock, PidAccountSup),
							pg2:join(pg2_all_client, CPid),
							CPid ! {event, start_client},
							self() ! {event, start},
							{noreply, State};
		{error, Reason} -> {stop, Reason, State}
	end;
handle_info({'EXIT', _Pid, closed}, State) ->
	{stop, State};
handle_info(Info, State) ->
	?INFO_MSG("get msg from handle_info/2 ~p ~p", [Info, State]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.


handle_cast(Msg, State) ->
	?INFO_MSG("get msg from handle_case/2 ~p ~p", [Msg, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.