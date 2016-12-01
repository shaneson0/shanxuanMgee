%%%----------------------------------------------------------------------
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-1-14
%%% @doc TODO: Add description to mgee_test_worker
%%% @end
%%%----------------------------------------------------------------------

-module(mgee_test_worker).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("mgee.hrl").
-include("game_pb.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record( tr_state,  {num, start_time, config, cur_state, pid1, pid2 } ).

%% ====================================================================
%% External functions
%% ====================================================================


start_link(N, Config) ->
	?DEBUG("module ~p start_link ~p ~p", [?MODULE, N, Config]),
	gen_server:start_link(?MODULE, [N, Config], []).

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
init([N, Config]) ->
	?DEBUG("module ~p init ~p ~p", [?MODULE, N, Config]),
	State = #tr_state{num=N, start_time=now(), config=Config, cur_state=stop},
    {ok, State}.

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

handle_cast(test_walk, State) ->
	CurState = State#tr_state.cur_state,
	if CurState =:= stop -> 
			Pid = walk(State),
			{noreply, State#tr_state{cur_state=walk, pid1 = Pid} };
	   true -> 
			State2 = stop(State),
			Pid = walk(State2),
			{noreply, State2#tr_state{cur_state=walk, pid1 = Pid} }
	end
	;

handle_cast(test_run, State) ->
	CurState = State#tr_state.cur_state,
	if CurState =:= stop -> 
			run(State),
			{noreply, State#tr_state{cur_state=run} };
	   true -> 
			stop(State),
			run(State),
			{noreply, State#tr_state{cur_state=run} }
	end
	;

handle_cast(test_fight, State) ->
	CurState = State#tr_state.cur_state,
	if CurState =:= stop -> 
			fight(State),
			{noreply, State#tr_state{cur_state=fight} };
	   true -> 
			stop(State),
			fight(State),
			{noreply, State#tr_state{cur_state=fight} }
	end
	;

handle_cast(test_stop, State) ->
	CurState = State#tr_state.cur_state,
	if CurState =/= stop -> 
			State2 = stop(State),
			{noreply, State2#tr_state{cur_state=stop} };
	   true -> 
			{noreply, State}
	end
	;

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

% begin test, connect to server, and loop.... test one person run...
run(State) ->
	?DEBUG("start test run, state is ~p", [State]),
	ok.


% begin test, connect to server, and loop.... test one person walk...
walk(State) ->
	?DEBUG("start test walk, state is ~p", [State]),
	N = State#tr_state.num,
	Config = State#tr_state.config,
	Pid = spawn_link(fun() -> mgee_test_walk:start( {N, Config} ) end ),	
	Pid.

% begin test, connect to server, and loop.... test two person fight together...
fight(State) ->
	?DEBUG("start test fight, state is ~p", [State]),
	ok.

% stop test, disconnect server, and stop loop....
stop(State) ->
	?DEBUG("stop test, state is ~p", [State]),
	ok.

