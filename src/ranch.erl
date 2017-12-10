-module(ranch).
-export([
	start_listener/5, 
	start_listener/6,
	stop_listener/1,
	child_spec/5, 
	child_spec/6,
	accept_ack/1,
	remove_connection/1,
	get_addr/1, get_port/1,
	get_max_connections/1, 
	set_max_connections/2,
	get_protocol_options/1, 
	set_protocol_options/2,
	info/0,
	procs/2,
	filter_options/3,
	set_option_default/3,
	require/1
]).

-deprecated([start_listener/6, child_spec/6]).
%%%% 这个监听器可以接受的最大连接数
-type max_conns() :: non_neg_integer() | infinity.
%%%% Ranch用这些参数来设置监听器
% ack_timeout(5000)----ranch:accept_ack/1的最大超时，默认是5000
% connection_type(worker)----处理连接的进程的类型，默认是worker
% max_connections(1024)----最大可用连接数，默认1024个
% num_acceptors(10)----接受连接的进程数，默认是10个
% shutdowm(5000)----
% socket----
-type opt() :: {ack_timeout, timeout()}
             | {connection_type, worker | supervisor}
			 | {max_connections, max_conns()}
			 | {num_acceptors, pos_integer()}
			 | {shutdown, timeout() | brutal_kill}
			 | {socket, any()}.
%% 监听器的UUID
-type ref() :: any().

-export_type([ref/0]).
-export_type([opt/0]).
-export_type([max_conns/0]).

%-spec accept_ack(ref()) -> ok.
% Ref: 监听器的UUID
% 表明连接已经被接受
% 这个函数必须由一个连接进程调用，用来告诉Ranch它已经正确初始化，
% 在socket能被安全使用前进行一些其他的操作
accept_ack(Ref) 
->
	receive {shoot, Ref, Transport, Socket, AckTimeout} ->
		Transport:accept_ack(Socket, AckTimeout)
	end.
%-spec child_spec(ref(), module(), any(), module(), any()) -> supervisor:child_spec().
% Ref----监听器名字
% NumAcceptors----接收器的数量
% Transport----传输模块
% TransOpts----传输选项
% Protocol----协议模块
% ProtoOpts----协议选项
% 返回一个子说明用于新监听器
% 可以利用这个函数直接在应用中嵌入监听器，而不用通过Ranch处理
child_spec(Ref, Transport, TransOpts, Protocol, ProtoOpts) 
->
	NumAcceptors = proplists:get_value(num_acceptors, TransOpts, 10),
	child_spec(Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts).
%-spec child_spec(ref(), non_neg_integer(), module(), any(), module(), any()) -> supervisor:child_spec().
child_spec(Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts)
when is_integer(NumAcceptors) 
andalso is_atom(Transport)
andalso is_atom(Protocol) 
->
	{{ranch_listener_sup, Ref}, 
	 {ranch_listener_sup, 
	  start_link, 
	  [Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts]
	 }, 
	 permanent, 
	 infinity, 
	 supervisor, 
	 [ranch_listener_sup]
	}.
%-spec get_addr(ref()) -> {inet:ip_address(), inet:port_number()}.
% Ref----监听器名字
% 返回{IP,Port}
% IP----监听器使用的IP
% Port----监听器使用的端口
% 获取给定监听器使用的IP地址和端口
get_addr(Ref) -> ranch_server:get_addr(Ref).	
%-spec get_port(ref()) -> inet:port_number().
% 返回给定监听器的端口
get_port(Ref) -> 
	{_, Port} = get_addr(Ref), 
	Port.
%-spec procs(ref(), acceptors | connections) -> [pid()].
% 以列表返回给定监听器的所有接收器或者连接进程的PID
procs(Ref, acceptors) -> procs1(Ref, ranch_acceptors_sup);
procs(Ref, connections) -> procs1(Ref, ranch_conns_sup).
procs1(Ref, Sup) 
->
	{_, ListenerSup, _, _} = lists:keyfind({ranch_listener_sup, Ref}, 1, supervisor:which_children(ranch_sup)),
	{_, SupPid, _, _} = lists:keyfind(Sup, 1, supervisor:which_children(ListenerSup)),
	[Pid || {_, Pid, _, _} <- supervisor:which_children(SupPid)].
%-spec set_max_connections(ref(), max_conns()) -> ok.
% 设置给定监听器的最大连接数，
% 设置会立即生效，如果设置的连接数比原来的连接数小，Ranch不会杀掉多余的连接，而是等它们自然的关闭
set_max_connections(Ref, MaxConnections) -> ranch_server:set_max_connections(Ref, MaxConnections).
%-spec get_max_connections(ref()) -> max_conns().
% 获取给定监听器的当前的最大连接数
get_max_connections(Ref) -> ranch_server:get_max_connections(Ref).
%-spec set_protocol_options(ref(), any()) -> ok.
% 设置给定监听器的协议选项，
% 设置会立即在新的连接上生效，以前的连接不会生效
set_protocol_options(Ref, Opts) -> ranch_server:set_protocol_options(Ref, Opts).
%-spec get_protocol_options(ref()) -> any().
% 获取给定监听器的当前协议选项
get_protocol_options(Ref) -> ranch_server:get_protocol_options(Ref).
%-spec remove_connection(ref()) -> ok.
% 这个函数只能从连接进程里调用，表示不给当前连接计数
% 可以将该函数用于那些大多数都是空闲而不是消费资源的长连接进程，这样可以使Ranch在不牺牲系统延时的情况下接收更多连接
remove_connection(Ref) 
->
	ConnsSup = ranch_server:get_connections_sup(Ref),
	ConnsSup ! {remove_connection, Ref, self()},
	ok.
%-spec start_listener(ref(), module(), any(), module(), any()) -> supervisor:startchild_ret().
% 用给定的传输模块和协议启动一个监听器，
% 返回该监听器的监督者的PID
start_listener(Ref, Transport, TransOpts, Protocol, ProtoOpts)
-> 
	NumAcceptors = proplists:get_value(num_acceptors, TransOpts, 10),
	start_listener(Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts).
%-spec start_listener(ref(), non_neg_integer(), module(), any(), module(), any()) -> supervisor:startchild_ret().
start_listener(Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts)
when is_integer(NumAcceptors)
andalso is_atom(Transport)
andalso is_atom(Protocol) 
->
	_ = code:ensure_loaded(Transport),
	case erlang:function_exported(Transport, name, 0) of
		false ->
			{error, badarg};
		true ->
			Res = supervisor:start_child(ranch_sup, child_spec(Ref, NumAcceptors, Transport, TransOpts, Protocol, ProtoOpts)),
			Socket = proplists:get_value(socket, TransOpts),
			case Res of
				{ok, Pid} when Socket =/= undefined ->
					%% Give ownership of the socket to ranch_acceptors_sup
					%% to make sure the socket stays open as long as the
					%% listener is alive. If the socket closes however there
					%% will be no way to recover because we don't know how
					%% to open it again.
					Children = supervisor:which_children(Pid),
					{_, AcceptorsSup, _, _} = lists:keyfind(ranch_acceptors_sup, 1, Children),
					%%% Note: the catch is here because SSL crashes when you change
					%%% the controlling process of a listen socket because of a bug.
					%%% The bug will be fixed in R16.
					catch Transport:controlling_process(Socket, AcceptorsSup);
				_ -> ok
			end,
			maybe_started(Res)
	end.
maybe_started({error, {{shutdown, {failed_to_start_child, ranch_acceptors_sup, {listen_error, _, Reason}}}, _}} = Error) 
	-> start_error(Reason, Error);
maybe_started(Res) -> Res.
start_error(E=eaddrinuse, _) -> {error, E};
start_error(E=eacces, _) -> {error, E};
start_error(E=no_cert, _) -> {error, E};
start_error(_, Error) -> Error.
%-spec stop_listener(ref()) -> ok | {error, not_found}.
% 停止给定的监听器
% 先关闭监听端口，再关闭连接进程
stop_listener(Ref) 
->
	case supervisor:terminate_child(ranch_sup, {ranch_listener_sup, Ref}) of
		ok ->
			_ = supervisor:delete_child(ranch_sup, {ranch_listener_sup, Ref}),
			ranch_server:cleanup_listener_opts(Ref);
		{error, Reason} -> {error, Reason}
	end.
%-spec info() -> [{any(), [{atom(), any()}]}].
% 返回所有Ranch监听器的详细信息，[ {监听器名字,[{信息名, 信息}]} ]
% 信息包括：
% {pid, ...}, 该监听器的顶级监督者的PID
% {ip, ...}, Ranch监听的IP地址
% {port, ...}, Ranch监听的端口
% {num_acceptors, ...}, 接收器进程数
% {max_connections, ...}, 最大连接数
% {active_connections, ...}, 活动的连接数
% {all_connections, ...}, 所有的连接数，包括从计数中移除的连接
% {transport, ...}, 传输模块
% {transport_options, ...}, 传输选项
% {protocol, ...}, 协议模块
% {protocol_options, ...} 协议选项
info() 
->
	Children = supervisor:which_children(ranch_sup),
	[{Ref, listener_info(Ref, Pid)} || {{ranch_listener_sup, Ref}, Pid, _, [_]} <- Children].
listener_info(Ref, Pid) 
->
	[_, NumAcceptors, Transport, TransOpts, Protocol, _] = listener_start_args(Ref),
	ConnsSup = ranch_server:get_connections_sup(Ref),
	{IP, Port} = get_addr(Ref),
	MaxConns = get_max_connections(Ref),
	ProtoOpts = get_protocol_options(Ref),
	[
		{pid, Pid},
		{ip, IP},
		{port, Port},
		{num_acceptors, NumAcceptors},
		{max_connections, MaxConns},
		{active_connections, ranch_conns_sup:active_connections(ConnsSup)},
		{all_connections, proplists:get_value(active, supervisor:count_children(ConnsSup))},
		{transport, Transport},
		{transport_options, TransOpts},
		{protocol, Protocol},
		{protocol_options, ProtoOpts}
	].
listener_start_args(Ref) 
->
	case erlang:function_exported(supervisor, get_childspec, 2) of
		true ->
			%% Can't use map syntax before R18.
			{ok, Map} = supervisor:get_childspec(ranch_sup, {ranch_listener_sup, Ref}),
			{ranch_listener_sup, start_link, StartArgs} = maps:get(start, Map),
			StartArgs;
		false ->
			%% Awful solution for compatibility with R16 and R17.
			{status, _, _, [_, _, _, _, [_, _, {data, [{_, {state, _, _, Children, _, _, _, _, _, _}}]}]]} = sys:get_status(ranch_sup),
			[StartArgs] = [StartArgs || {child, _, {ranch_listener_sup, ChildRef}, {ranch_listener_sup, start_link, StartArgs}, _, _, _, _} <- Children, ChildRef =:= Ref],
			StartArgs
	end.



%-spec filter_options([inet | inet6 | {atom(), any()} | {raw, any(), any(), any()}], [atom()], Acc) -> Acc when Acc :: [any()].
filter_options(UserOptions, DisallowedKeys, DefaultOptions) 
->
	AllowedOptions = filter_user_options(UserOptions, DisallowedKeys),
	lists:foldl(fun merge_options/2, DefaultOptions, AllowedOptions).
%% 2-tuple options.
filter_user_options([Opt = {Key, _}|Tail], DisallowedKeys) 
->
	case lists:member(Key, DisallowedKeys) of
		false ->
			[Opt|filter_user_options(Tail, DisallowedKeys)];
		true ->
			filter_options_warning(Opt),
			filter_user_options(Tail, DisallowedKeys)
	end;
%% Special option forms.
filter_user_options([inet|Tail], AllowedKeys) -> [inet|filter_user_options(Tail, AllowedKeys)];
filter_user_options([inet6|Tail], AllowedKeys) -> [inet6|filter_user_options(Tail, AllowedKeys)];
filter_user_options([Opt = {raw, _, _, _}|Tail], AllowedKeys) -> [Opt|filter_user_options(Tail, AllowedKeys)];
filter_user_options([Opt|Tail], DisallowedKeys) -> filter_options_warning(Opt), filter_user_options(Tail, DisallowedKeys);
filter_user_options([], _) -> [].

filter_options_warning(Opt) -> error_logger:warning_msg("Transport option ~p unknown or invalid.~n", [Opt]).

merge_options({Key, _} = Option, OptionList) -> lists:keystore(Key, 1, OptionList, Option);
merge_options(Option, OptionList) -> [Option|OptionList].

%-spec set_option_default(Opts, atom(), any()) -> Opts when Opts :: [{atom(), any()}].
set_option_default(Opts, Key, Value) 
->
	case lists:keymember(Key, 1, Opts) of
		true -> Opts;
		false -> [{Key, Value}|Opts]
	end.

%-spec require([atom()]) -> ok.
require([]) -> ok;
require([App|Tail]) 
->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Tail).
