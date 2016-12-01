%% Author: Administrator
%% Created: 2010-1-3
%% Description: TODO: Add description to mgee_packet_decode
-module(mgee_packet).

%%
%% Include files
%%
-include("mgee.hrl").
%%
%% Exported Functions
%%
-export([
		 recv/1, 
		 send/2, 
		 recv/2, 
		 packet_encode_send/4
  ]).

-export([
		 packet/3, 
		 unpack/1,
		 get_decode_func/2,
		 get_encode_func/2,
		 encode/3,
		 decode/3
		]).

%% here we don't care the cross domain file
recv(ClientSock) ->
	case gen_tcp:recv(ClientSock, 2) of

		{ok, PacketLenBin} -> <<PacketLen:16>> = PacketLenBin,

			?DEBUG("PacketLenBin : ~p", [PacketLen]),

							  case gen_tcp:recv(ClientSock, PacketLen) of
								  {ok, RealData} ->
									  ?DEBUG("recv data ~p", [RealData]),
									  {ok, mgee_packet:unpack(RealData)};
								  {error, Reason} ->
									  ?ERROR_MSG("read packet data failed with reason: ~p", [Reason]),
									  {error, Reason}
							  end;
		{error, Reason} -> ?ERROR_MSG("read packet length failed with reason: ~p", [Reason]),
						   {error, Reason}
	end.

%% @desc sometime we need the Timeout option
recv(ClientSock, Timeout) ->
	case gen_tcp:recv(ClientSock, 2, Timeout) of
		{ok, PacketLenBin} -> <<PacketLen:16>> = PacketLenBin,

			?DEBUG("PacketLenBin : ~p", [PacketLen]),

							  case gen_tcp:recv(ClientSock, PacketLen, Timeout) of
								  {ok, RealData} ->
									  ?DEBUG("recv data ~p", [RealData]),
									  {ok, mgee_packet:unpack(RealData)};
								  {error, Reason} ->
									  ?ERROR_MSG("read packet data failed with reason: ~p", [Reason]),
									  {error, Reason}
							  end;
		{error, Reason} -> ?ERROR_MSG("read packet length failed with reason: ~p", [Reason]),
						   {error, Reason}
	end.

packet_encode_send(ClientSock, Module, Method, DataRecord) ->
	DataBin = encode(Module, Method, DataRecord),

	?DEBUG("Module : ~p Method : ~p , DataBin : ~p , DataRecord : ~p ~n ",[Module ,Method , DataBin , DataRecord ]),
send(ClientSock, mgee_packet:packet(Module, Method, DataBin)).

send(ClientSock, Bin) ->
	?DEBUG("packet send ~p ", [Bin]),
	PacketLen = erlang:byte_size(Bin),
	SendBin = <<PacketLen:16, Bin/binary>>,
	case gen_tcp:send(ClientSock, SendBin) of
		ok -> ?DEBUG("!!!packet send ~p ok ", [SendBin]),ok;
		{error, closed} -> {error, closed};
		{error, Reason} -> {error, Reason}
	end.

packet(Module, Method, Data) when is_list(Module) and is_list(Method) ->
	Module2 = list_to_binary(Module),
	Method2 = list_to_binary(Method),
	packet2(Module2, Method2, Data);
packet(Module, Method, Data) when is_list(Module) ->
	Module2 = list_to_binary(Module),
	packet2(Module2, Method, Data);
packet(Module, Method, Data) when is_list(Method) ->
	Method2 = list_to_binary(Method),
	packet2(Module, Method2, Data);
packet(Module, Method, Data) when is_binary(Module) and is_binary(Method) ->
	packet2(Module, Method, Data).

packet2(Module, Method, Data) ->
	ModuleLen = erlang:length(binary_to_list(Module)),
	MethodLen = erlang:length(binary_to_list(Method)),



%%	DataCompress = zlib:compress(Data),

	<<ModuleLen:8, MethodLen:8, Module/binary, Method/binary, Data/binary>>.

unpack(DataRaw) ->

	?DEBUG("DataRaw data ~p", [DataRaw]),

	<<ModuleLen:8, MethodLen:8, Bin/binary>> = DataRaw,

	?DEBUG("ModuleLen :  ~p ~n , MethodLen : ~p ~n, Bin : ~p Bin_to_list : ~p ~n", [ModuleLen,MethodLen,Bin , binary_to_list(Bin)]),

	<<Module:ModuleLen/binary, Bin2/binary>> = Bin,
	<<Method:MethodLen/binary, Data/binary>> = Bin2,

	?DEBUG("Module :  ~p ~n , Method : ~p ~n Data : ~p , binary_to_list : ~p ~n", [Module,Method , Data,binary_to_list(Data)]),

	<<Flag:1/binary , _OtherData/binary>> = Module ,

	if
		Flag =:= 1 -> ResponseData = zlib:uncompress(Data);
		true -> 			ResponseData = Data
	end,
	{Module, Method, ResponseData}.



get_decode_func(Module, Method) ->
	mgee_misc:list_to_atom2(
		lists:concat(
			[decode_m_, mgee_tool:to_list(Module), "_",mgee_tool:to_list(Method), "_tos"])).

get_encode_func(Module, Method) ->
  	mgee_misc:list_to_atom2(
		lists:concat(
			[encode_m_, mgee_tool:to_list(Module), "_",mgee_tool:to_list(Method), "_toc"])).

encode(Module, Method, DataRecord) ->
	apply(game_pb, get_encode_func(Module, Method), [DataRecord]).

decode(Module, Method, DataBin) ->
	apply(game_pb, mgee_packet:get_decode_func(Module, Method), [DataBin]).