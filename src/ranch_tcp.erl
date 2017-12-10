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

-module(ranch_tcp).
-behaviour(ranch_transport).
%% 所有的callback都实现了
-export([name/0]).
-export([secure/0]).
-export([messages/0]).
-export([listen/1]).
-export([disallowed_listen_options/0]).
-export([accept/2]).
-export([accept_ack/2]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).
-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([shutdown/2]).
-export([close/1]).

-type opt() :: {backlog, non_neg_integer()}
			 | {buffer, non_neg_integer()}
			 | {delay_send, boolean()}
			 | {dontroute, boolean()}
			 | {exit_on_close, boolean()}
			 | {fd, non_neg_integer()}
			 | {high_msgq_watermark, non_neg_integer()}
			 | {high_watermark, non_neg_integer()}
		 	 | inet
			 | inet6
			 | {ip, inet:ip_address()}
			 | {ipv6_v6only, boolean()}
			 | {keepalive, boolean()}
			 | {linger, {boolean(), non_neg_integer()}}
			 | {low_msgq_watermark, non_neg_integer()}
			 | {low_watermark, non_neg_integer()}
			 | {nodelay, boolean()}
			 | {port, inet:port_number()}
			 | {priority, integer()}
			 | {raw, non_neg_integer(), non_neg_integer(), binary()}
			 | {recbuf, non_neg_integer()}
			 | {send_timeout, timeout()}
			 | {send_timeout_close, boolean()}
			 | {sndbuf, non_neg_integer()}
			 | {tos, integer()}.
-export_type([opt/0]).

-type opts() :: [opt()].
-export_type([opts/0]).
%% 实现name/0
%% 返回传输层名字
%% TCP传输
name() -> tcp.
%% 实现secure/0
secure() -> false.
%% 实现messages/0
%% 返回在主动模式下发送的消息的标识
messages() -> {tcp, tcp_closed, tcp_error}.
%% 实现listen/1
%% 在指定端口监听连接，
%% 返回的是监听套接字，可以用来接受连接，不可能用这个套接字来接收或发送数据
%% -spec listen(opts()) -> {ok, LSocket::inet:socket()} | {error, atom()}.
listen(Opts) ->
	Opts2 = ranch:set_option_default(Opts, backlog, 1024),
	Opts3 = ranch:set_option_default(Opts2, nodelay, true),
	Opts4 = ranch:set_option_default(Opts3, send_timeout, 30000),
	Opts5 = ranch:set_option_default(Opts4, send_timeout_close, true),
	%% We set the port to 0 because it is given in the Opts directly.
	%% The port in the options takes precedence over the one in the
	%% first argument.
	gen_tcp:listen(
		0, 
		ranch:filter_options(
			Opts5, 
			disallowed_listen_options(),
			[binary, {active, false}, {packet, raw}, {reuseaddr, true}]
		)
	).
%% 'binary' and 'list' are disallowed but they are handled
%% specifically as they do not have 2-tuple equivalents.
disallowed_listen_options() ->
	[active, header, mode, packet, packet_size, line_delimiter, reuseaddr].
%% 实现accept/2
%% -spec accept(LSocket::inet:socket(), timeout()) -> {ok, CSocket::inet:socket()} | {error, closed | timeout | atom()}.
%% 在给定监听套接字接受连接，返回客户端套接字，
%% 接受完连接后，使用accept_ack/2来初始化客户端套接字，
%% 这在不是原始TCP传输时非常有用，例如带SSL的TCP传输
accept(LSocket, Timeout) -> gen_tcp:accept(LSocket, Timeout).
%% 实现accept_ack/2
%% -spec accept_ack(CSocket::inet:socket(), timeout()) -> ok.
accept_ack(_, _) -> ok.
%% 实现close/1
%% -spec close(inet:socket()) -> ok.
%% 关闭套接字
close(Socket) -> gen_tcp:close(Socket).
%% 实现connect/3
%% -spec connect(inet:ip_address() | inet:hostname(), inet:port_number(), any()) -> {ok, inet:socket()} | {error, atom()}.
%% @todo Probably filter Opts?
connect(Host, Port, Opts) when is_integer(Port) ->
	gen_tcp:connect(Host, Port, Opts ++ [binary, {active, false}, {packet, raw}]).
%% 实现connect/4
%% -spec connect(inet:ip_address() | inet:hostname(), inet:port_number(), any(), timeout()) -> {ok, inet:socket()} | {error, atom()}.
%% @todo Probably filter Opts?
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	gen_tcp:connect(Host, Port, Opts ++ [binary, {active, false}, {packet, raw}], Timeout).
%% 实现controlling_process/2
%% -spec controlling_process(inet:socket(), pid()) -> ok | {error, closed | not_owner | atom()}.
%% 改变套接字的控制进程，
%% 控制进程是可以在套接字上进行操作的进程，在主动模式下，还可以从套接字接收消息，如果控制进程死了，套接字就会关闭
%% Socket是由listen/1或者accept/2打开的套接字
%% PID是套接字的新持有者的PID
controlling_process(Socket, Pid) -> gen_tcp:controlling_process(Socket, Pid).
%% 实现recv/3
%% -spec recv(CSocket::inet:socket(), Length::non_neg_integer(), timeout()) -> {ok, any()} | {error, closed | atom()}.
%% 在被动模式下，从客户端套接字接收数据，如果在主动模式下调用这个函数接收数据，会返回错误，
%% 如果长度设置为0，会返回套接字上所有的可用数据，
%% 不建议把超时设置为infinity，如果套接字没有数据来，进程就会被饿死，
%% 比如当远程端点崩溃后(客户端挂掉，没有正确发送套接字关闭信息)，客户端套接字变成半开，这个时候进程就被饿死了
recv(CSocket, Length, Timeout) -> gen_tcp:recv(CSocket, Length, Timeout).
%% 实现send/2
%% -spec send(CSocket::inet:socket(), Packet::iodata()) -> ok | {error, atom()}.
%% 用给定套接字发送数据
%% Packet：要发送的数据
send(CSocket, Packet) -> gen_tcp:send(CSocket, Packet).
%% @todo Probably filter Opts?
%% -spec setopts(CSocket::inet:socket(), SockOpts::list()) -> ok | {error, atom()}.
%% 改变给定套接字的选项，
%% 这个函数主要用于切换套接字的主动模式和被动模式，或者设置某些协议特有的选项
setopts(CSocket, Opts) -> inet:setopts(CSocket, Opts).
%5 实现peername/1
%% -spec peername(CSocket::inet:socket()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
%% 返回远程端点的IP和Port
peername(CSocket) -> inet:peername(CSocket).
%% 实现sockname/1
%% -spec sockname(Socket::inet:socket()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
%% 返回本地端点的IP和Port
sockname(Socket) -> inet:sockname(Socket).
%% 实现shutdown/2
%% -spec shutdown(CSocket::inet:socket(), How::read | write | read_write) -> ok | {error, atom()}.
%% 立即关闭套接字(单向或双向)
shutdown(CSocket, How) -> gen_tcp:shutdown(CSocket, How).
%% 实现sendfile/2
%% -spec sendfile(inet:socket(), file:name_all() | file:fd()) -> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename) -> sendfile(Socket, Filename, 0, 0, []).
%% 实现sendfile/4
%% -spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(), non_neg_integer()) -> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes) -> sendfile(Socket, File, Offset, Bytes, []).
%% 实现sendfile/5
%% -spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(), non_neg_integer(), [{chunk_size, non_neg_integer()}]) -> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename, Offset, Bytes, Opts) when is_list(Filename) 
												orelse is_atom(Filename)
												orelse is_binary(Filename)
->	case file:open(Filename, [read, raw, binary]) of
		{ok, RawFile} ->
			try sendfile(Socket, RawFile, Offset, Bytes, Opts) of
				Result -> Result
			after
				ok = file:close(RawFile)
			end;
		{error, _} = Error -> Error
	end;
sendfile(Socket, RawFile, Offset, Bytes, Opts) ->
	Opts2 = case Opts of 
				[] -> [{chunk_size, 16#1FFF}];
				_ -> Opts
			end,
	try file:sendfile(RawFile, Socket, Offset, Bytes, Opts2) of
		Result -> Result
	catch
		error:{badmatch, {error, enotconn}} -> {error, closed}
			%% file:sendfile/5 might fail by throwing a
			%% {badmatch, {error, enotconn}}. This is because its
			%% implementation fails with a badmatch in
			%% prim_file:sendfile/10 if the socket is not connected.
	end.