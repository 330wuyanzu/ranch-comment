%% 这个模块是behavior，
%% 定义的是传输模块的接口
-module(ranch_transport).

-export([sendfile/6]).

-type socket() :: any().
-type opts() :: any().
-type sendfile_opts() :: [{chunk_size, non_neg_integer()}].
-export_type([sendfile_opts/0]).

%% 返回传输层的名字
-callback name() -> Name::atom().
%% 
-callback secure() -> boolean().
%% 返回在主动模式下发送的消息的标识
-callback messages() -> {OK::atom(), Closed::atom(), Error::atom()}.
%% 在指定端口监听连接，
%% 返回的是监听套接字，可以用来接受连接，不可能用这个套接字来接收或发送数据
-callback listen(TransOpts::opts()) -> {ok, LSocket::socket()} | {error, atom()}.
%% 在给定监听套接字接受连接，返回客户端套接字，
%% 接受完连接后，使用accept_ack/2来初始化客户端套接字，
%% 这在不是原始TCP传输时非常有用，例如带SSL的TCP传输
-callback accept(LSocket::socket(), timeout()) -> {ok, CSocket::socket()} | {error, closed | timeout | atom()}.
%% 执行快速初始化，
%% 在进行任何客户端套接字的操作前，连接进程要调用这个函数
%% 这个函数允许传输模块执行额外的初始化工作
-callback accept_ack(CSocket::socket(), Timeout::timeout()) -> ok.
%% 关闭给定的套接字(由listen/1或者accept/2打开的)
-callback close(Socket::socket()) -> ok.
%%
-callback connect(string(), inet:port_number(), opts()) -> {ok, socket()} | {error, atom()}.
%%
-callback connect(string(), inet:port_number(), opts(), timeout()) -> {ok, socket()} | {error, atom()}.
%% 改变套接字的控制进程，
%% 控制进程是可以在套接字上进行操作的进程，在主动模式下，还可以从套接字接收消息，如果控制进程死了，套接字就会关闭
%% Socket是由listen/1或者accept/2打开的套接字
%% PID是套接字的新持有者的PID
-callback controlling_process(Socket::socket(), PID::pid()) -> ok | {error, closed | not_owner | atom()}.
%% 在被动模式下，从客户端套接字接收数据，如果在主动模式下调用这个函数接收数据，会返回错误，
%% 如果长度设置为0，会返回套接字上所有的可用数据，
%% 不建议把超时设置为infinity，如果套接字没有数据来，进程就会被饿死，
%% 比如当远程端点崩溃后(客户端挂掉，没有正确发送套接字关闭信息)，客户端套接字变成半开，这个时候进程就被饿死了
-callback recv(CSocket::socket(), Length::non_neg_integer(), Timeout::timeout()) -> {ok, Packet::any()} | {error, closed | timeout | atom()}.
%% 用给定套接字发送数据
%% Packet：要发送的数据
-callback send(CSsocket::socket(), Packet::iodata()) -> ok | {error, atom()}.
%% 改变给定套接字的选项，
%% 这个函数主要用于切换套接字的主动模式和被动模式，或者设置某些协议特有的选项
-callback setopts(CSocket::socket(), SockOpts::opts()) -> ok | {error, atom()}.
%% 返回远程端点的IP和Port
-callback peername(CSocket::socket()) -> {ok, {IP::inet:ip_address(), Port::inet:port_number()}} | {error, atom()}.
%% 返回本地端点的IP和Port
-callback sockname(Socket::socket()) -> {ok, {IP::inet:ip_address(), Port::inet:port_number()}} | {error, atom()}.
%% 立即关闭套接字(单向或双向)
-callback shutdown(CSocket::socket(), How::read | write | read_write) -> ok | {error, atom()}.



%% 可以不用实现，直接调用下面的sendfile发送文件
-callback sendfile(socket(), file:name() | file:fd()) -> {ok, non_neg_integer()} | {error, atom()}.
-callback sendfile(socket(), file:name() | file:fd(), non_neg_integer(), non_neg_integer()) -> {ok, non_neg_integer()} | {error, atom()}.
-callback sendfile(socket(), file:name() | file:fd(), non_neg_integer(), non_neg_integer(), sendfile_opts()) -> {ok, non_neg_integer()} | {error, atom()}.
%% A fallback for transports that don't have a native sendfile implementation.
%% Note that the ordering of arguments is different from file:sendfile/5 and
%% that this function accepts either a raw file or a file name.
-spec sendfile(module(), socket(), file:filename_all() | file:fd(),
		non_neg_integer(), non_neg_integer(), sendfile_opts())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Transport, Socket, Filename, Offset, Bytes, Opts)
		when is_list(Filename) orelse is_atom(Filename)
		orelse is_binary(Filename) ->
	ChunkSize = chunk_size(Opts),
	case file:open(Filename, [read, raw, binary]) of
		{ok, RawFile} ->
			_ = case Offset of
				0 ->
					ok;
				_ ->
					{ok, _} = file:position(RawFile, {bof, Offset})
			end,
			try
				sendfile_loop(Transport, Socket, RawFile, Bytes, 0, ChunkSize)
			after
				ok = file:close(RawFile)
			end;
		{error, _Reason} = Error ->
			Error
	end;
sendfile(Transport, Socket, RawFile, Offset, Bytes, Opts) ->
	ChunkSize = chunk_size(Opts),
	Initial2 = case file:position(RawFile, {cur, 0}) of
		{ok, Offset} ->
			Offset;
		{ok, Initial} ->
			{ok, _} = file:position(RawFile, {bof, Offset}),
			Initial
		end,
	case sendfile_loop(Transport, Socket, RawFile, Bytes, 0, ChunkSize) of
		{ok, _Sent} = Result ->
			{ok, _} = file:position(RawFile, {bof, Initial2}),
			Result;
		{error, _Reason} = Error ->
			Error
	end.

-spec chunk_size(sendfile_opts()) -> pos_integer().
chunk_size(Opts) ->
	case lists:keyfind(chunk_size, 1, Opts) of
		{chunk_size, ChunkSize}
				when is_integer(ChunkSize) andalso ChunkSize > 0 ->
			ChunkSize;
		{chunk_size, 0} ->
			16#1FFF;
		false ->
			16#1FFF
	end.

-spec sendfile_loop(module(), socket(), file:fd(), non_neg_integer(),
		non_neg_integer(), pos_integer())
	-> {ok, non_neg_integer()} | {error, term()}.
sendfile_loop(_Transport, _Socket, _RawFile, Sent, Sent, _ChunkSize)
		when Sent =/= 0 ->
	%% All requested data has been read and sent, return number of bytes sent.
	{ok, Sent};
sendfile_loop(Transport, Socket, RawFile, Bytes, Sent, ChunkSize) ->
	ReadSize = read_size(Bytes, Sent, ChunkSize),
	case file:read(RawFile, ReadSize) of
		{ok, IoData} ->
			case Transport:send(Socket, IoData) of
				ok ->
					Sent2 = iolist_size(IoData) + Sent,
					sendfile_loop(Transport, Socket, RawFile, Bytes, Sent2,
						ChunkSize);
				{error, _Reason} = Error ->
					Error
			end;
		eof ->
			{ok, Sent};
		{error, _Reason} = Error ->
			Error
	end.

-spec read_size(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
	non_neg_integer().
read_size(0, _Sent, ChunkSize) ->
	ChunkSize;
read_size(Bytes, Sent, ChunkSize) ->
	min(Bytes - Sent, ChunkSize).
