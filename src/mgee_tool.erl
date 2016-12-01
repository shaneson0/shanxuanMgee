%%%----------------------------------------------------------------------
%%%
%%% @copyright 2010 mgee (Ming Game Engine Erlang)
%%%
%%% @author odinxu, 2010-01-11
%%% @doc mgee common tools function
%%% @end
%%%
%%%----------------------------------------------------------------------

-module(mgee_tool).

-compile(export_all).


-export([
		 ip/1,
		 sort/1,
		 for/3,	
		 f2s/1,	 
		 get_type/2,
		 list_random/1,
		 random_dice/2,
		 random/2,
		 odds/2,
		 odds_list/1,
		 odds_list/2,
		 odds_list_sum/1,
		 ceil/1,
		 floor/1,
		 sleep/1,
		 subatom/2,
		 to_integer/1,
		 to_binary/1,
		 to_tuple/1,
		 to_float/1,
		 to_list/1,
		 to_atom/1
		]).

%% @doc get IP address string from Socket
ip(Socket) ->
  	{ok, {IP, _Port}} = inet:peername(Socket),
  	{Ip0,Ip1,Ip2,Ip3} = IP,
	list_to_binary(integer_to_list(Ip0)++"."++integer_to_list(Ip1)++"."++integer_to_list(Ip2)++"."++integer_to_list(Ip3)).


%% @doc quick sort
sort([]) ->
	[];
sort([H|T]) -> 
	sort([X||X<-T,X<H]) ++ [H] ++ sort([X||X<-T,X>=H]).

%% for
for(Max,Max,F)->[F(Max)];
for(I,Max,F)->[F(I)|for(I+1,Max,F)].


%% @doc convert float to string,  f2s(1.5678) -> 1.57
f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
    [A] = io_lib:format("~.2f", [F]),
	A.


%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) -> 
    Msg;
to_atom(Msg) when is_binary(Msg) -> 
	list_to_atom(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) -> 
    list_to_atom(Msg);
to_atom(_) -> 
    throw(other_value).  %%list_to_atom("").

%% @doc convert other type to list
to_list(Msg) when is_list(Msg) -> 
    Msg;
to_list(Msg) when is_atom(Msg) -> 
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) -> 
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) -> 
    integer_to_list(Msg);
to_list(Msg) when is_float(Msg) -> 
    f2s(Msg);
to_list(_) ->
    throw(other_value).

%% @doc convert other type to binary
to_binary(Msg) when is_binary(Msg) -> 
    Msg;
to_binary(Msg) when is_atom(Msg) ->
	list_to_binary(atom_to_list(Msg));
	%%atom_to_binary(Msg, utf8);
to_binary(Msg) when is_list(Msg) -> 
	list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) -> 
	list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) -> 
	list_to_binary(f2s(Msg));
to_binary(_Msg) ->
    throw(other_value).

%% @doc convert other type to float
to_float(Msg)->
	Msg2 = to_list(Msg),
	list_to_float(Msg2).

%% @doc convert other type to integer
-spec to_integer(Msg :: any()) -> integer().
to_integer(Msg) when is_integer(Msg) -> 
    Msg;
to_integer(Msg) when is_binary(Msg) ->
	Msg2 = binary_to_list(Msg),
    list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) -> 
    list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) -> 
    round(Msg);
to_integer(_Msg) ->
    throw(other_value).

%% @doc convert other type to tuple
to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) -> {T}.

%% @doc get data type {0=integer,1=list,2=atom,3=binary}
get_type(DataValue,DataType)->
	case DataType of
		0 ->
			DataValue2 = binary_to_list(DataValue),
			list_to_integer(DataValue2);
		1 ->
			binary_to_list(DataValue);
		2 ->
			DataValue2 = binary_to_list(DataValue),
			list_to_atom(DataValue2);
		3 -> 
			DataValue
	end.



%% @doc get random list
list_random(List)->
	case List of
		[] ->
			{};
		_ ->
			RS			=	lists:nth(random:uniform(length(List)), List),
			ListTail	= 	lists:delete(RS,List),
			{RS,ListTail}
	end.

%% @doc get a random integer between Min and Max
random(Min,Max)->
	Min2 = Min-1,
	random:uniform(Max-Min2)+Min2.

%% @doc 掷骰子
random_dice(Face,Times)->
	if
		Times == 1 ->
			random(1,Face);
		true ->
			lists:sum(for(1,Times, fun(_)-> random(1,Face) end))
	end.

%% @doc 机率
odds(Numerator,Denominator)->
	Odds = random:uniform(Denominator),
	if
		Odds =< Numerator -> 
			true;
		true ->
			false
	end.
odds_list(List)->
	Sum = odds_list_sum(List),
	odds_list(List,Sum).
odds_list([{Id,Odds}|List],Sum)->
	case odds(Odds,Sum) of
		true ->
			Id;
		false ->
			odds_list(List,Sum-Odds)
	end.
odds_list_sum(List)->
	{_List1,List2} = lists:unzip(List),
	lists:sum(List2).


%% @doc 取整 大于X的最小整数
ceil(X) ->
    T = trunc(X),
	if 
		X - T == 0 ->
			T;
		true ->
			if
				X > 0 ->
					T + 1;
				true ->
					T
			end			
	end.


%% @doc 取整 小于X的最大整数
floor(X) ->
    T = trunc(X),
	if 
		X - T == 0 ->
			T;
		true ->
			if
				X > 0 ->
					T;
				true ->
					T-1
			end
	end.
%% 4舍5入
%% round(X)

%% subatom
subatom(Atom,Len)->	
	list_to_atom(lists:sublist(atom_to_list(Atom),Len)).

%% @doc 暂停多少毫秒
sleep(Msec) ->
	receive
		after Msec ->
			true
	end.

