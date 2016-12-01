%%%----------------------------------------------------------------------
%%% File    : mgee_misc.erl
%%% Author  : Qingliang
%%% Created : 2010-01-02
%%% Description: Ming game engine erlang
%%%----------------------------------------------------------------------

-module(mgee_misc).

%%
%% Include files
%%
-include("mgee.hrl").
%%
%% Exported Functions
%%
-export([manage_applications/6, start_applications/1, stop_applications/1, tcp_name/3]).

-export([
		 account_process_name/1,
		 role_process_name/1, 
		 whereis_name/1, 
		 register/3, 
		 get_account_pid/1, 
		 md5/1,
		 get_role_pid/1
		]).

-export([list_to_atom2/1,get_socket_by_roleid/1,get_socket_by_rolepid/1]).

%%
%% API Functions
%%

manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                    case Do(App) of
                        ok -> [App | Acc];
                        {error, {SkipError, _}} -> Acc;
                        {error, Reason} ->
                            lists:foreach(Undo, Acc),
                            throw({error, {ErrorTag, App, Reason}})
                    end
            end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
                        fun application:start/1,
                        fun application:stop/1,
                        already_started,
                        cannot_start_application,
                        Apps).

stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
                        fun application:stop/1,
                        fun application:start/1,
                        not_started,
                        cannot_stop_application,
                        Apps).

tcp_name(Prefix, IPAddress, Port)
  when is_atom(Prefix) andalso is_number(Port) ->
    list_to_atom2(
      lists:flatten(
        io_lib:format("~w_~s:~w",
                      [Prefix, inet_parse:ntoa(IPAddress), Port]))).

%% don't care about chinese, it performance well.
account_process_name(AccountName) when is_integer(AccountName) or is_atom(AccountName) ->
	list_to_atom2(
	  lists:concat([mgee_account_, AccountName]));
account_process_name(AccountName) when is_list(AccountName) ->
	list_to_atom2(
	  lists:flatten(["mgee_account_"|AccountName]));
account_process_name(AccountName) when is_binary(AccountName) ->
	list_to_atom2(
		lists:concat([mgee_account_,mgee_misc:md5(AccountName)])).

role_process_name(Roleid) when is_integer(Roleid) or is_atom(Roleid) ->
	list_to_atom2(
	  lists:concat([mgee_role_, Roleid]));
role_process_name(Roleid) when is_list(Roleid) ->
	list_to_atom2(
	  lists:flatten(["mgee_role_"|Roleid]));
role_process_name(Roleid) when is_binary(Roleid) ->
	list_to_atom2(
		lists:concat([mgee_role_,mgee_misc:md5(Roleid)])).

get_account_pid(AccountName) ->
	mgee_misc:whereis_name({local, mgee_misc:account_process_name(AccountName)}).

get_role_pid(Roleid)  ->
	 mgee_misc:whereis_name({local, mgee_misc:role_process_name(Roleid)}).

%% get the pid of a registered name
whereis_name({local, Atom}) -> 
	Pid = erlang:whereis(Atom),
	?INFO_MSG("find local ~p ~p", [Pid, Atom]),
	Pid;
whereis_name({global, Atom}) ->
	global:whereis_name(Atom).

register(local, Name, Pid) ->
	erlang:register(Name, Pid);
register(global, Name, Pid) ->
	global:register_name(Name, Pid).

md5(S) ->        
	Md5_bin =  erlang:md5(S), 
    Md5_list = binary_to_list(Md5_bin), 
    lists:flatten(list_to_hex(Md5_list)). 
 
list_to_hex(L) -> 
	lists:map(fun(X) -> int_to_hex(X) end, L). 
 
int_to_hex(N) when N < 256 -> 
    [hex(N div 16), hex(N rem 16)]. 
hex(N) when N < 10 -> 
       $0+N; 
hex(N) when N >= 10, N < 16 ->      
	$a + (N-10).

list_to_atom2(List) when is_list(List) ->
	case catch(list_to_existing_atom(List)) of
		{'EXIT', _} -> erlang:list_to_atom(List);
		Atom when is_atom(Atom) -> Atom
	end.

%%补充

get_socket_by_roleid(Roleid) ->
	Pid = mgee_misc:get_role_pid(Roleid),
	%%这个表存储了{Pid , Client , _}信息
	case ets:lookup(?ETS_IN_VW_ROLE_LIST, Pid) of
		[{Pid, ClientSock, _}] ->
			{ok, ClientSock};
		_ ->
			{error, not_found}
	end.

get_socket_by_rolepid(Pid) ->
	case ets:lookup(?ETS_IN_VW_ROLE_LIST, Pid) of
		[{Pid, ClientSock, _}] ->
			{ok, ClientSock};
		_ ->
			{error, not_found}
	end.














