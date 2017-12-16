%% Copyright (c) 2011-2017, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% 监听者是一堆在指定端口监听新TCP连接的进程，
%% 监听者管理着一个接收器进程池，每一个接收器进程不确定的接收连接，
%% 当接收一个连接时，监听者会启动一个新进程来执行协议处理代码，
%% 所有的套接字编程通过使用传输层处理器来抽象出来
%% 监听者掌管接收器和连接进程，使开发者专注于应用开发，不必为这些琐事担心。
-module(ranch_listener_sup).
-behaviour(supervisor).

-export([start_link/6]).
-export([init/1]).

%% API
%-spec start_link(ranch:ref(), non_neg_integer(), module(), any(), module(), any()) -> {ok, pid()}.
start_link(Listener_Name, NumAcceptors, Transport_Module, TransOpts, Protocol_Module, ProtoOpts) ->
	% proplists:get_value(Key, List, Default) -> Value | Default
	% 在一个键值列表查找Key，返回相应的Value，否则返回Default
	MaxConns = proplists:get_value(max_connections, TransOpts, 1024),
	ranch_server:set_new_listener_opts(Listener_Name, MaxConns, ProtoOpts),
	CallbackModule = ranch_listener_sup,
	InitArgs = {Listener_Name, NumAcceptors, Transport_Module, TransOpts, Protocol_Module},
	supervisor:start_link(CallbackModule, InitArgs).

%% Callback
init({ListenerName, NumAcceptors, TransportModule, TransOpts, ProtocolModule}) ->
	AckTimeout = proplists:get_value(ack_timeout, TransOpts, 5000),
	ConnType = proplists:get_value(connection_type, TransOpts, worker),
	Shutdown = proplists:get_value(shutdown, TransOpts, 5000),
	ConnsSupSpec = #{
		id => ranch_conns_sup,
		start => {ranch_conns_sup, create_connection_supervisor, [ListenerName, ConnType, Shutdown, TransportModule, AckTimeout, ProtocolModule]},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [ranch_conns_sup]
	},
	AcceptorsSupSpec = #{
		id => ranch_acceptors_sup,
		start => {ranch_acceptors_sup, start_link, [ListenerName, NumAcceptors, TransportModule, TransOpts]},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [ranch_acceptors_sup]
	},
	ChildSpecs = [ConnsSupSpec, AcceptorsSupSpec],
	{ok, {{rest_for_one, 1, 5}, ChildSpecs}}.
