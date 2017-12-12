%% Copyright (c) 2012-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_server).
-behaviour(gen_server).

%% API.
-export([start_server/0]).
-export([set_new_listener_opts/3]).
-export([cleanup_listener_opts/1]).
-export([set_connections_sup/2]).
-export([get_connections_sup/1]).
-export([set_addr/2]).
-export([get_addr/1]).
-export([set_max_connections/2]).
-export([get_max_connections/1]).
-export([set_protocol_options/2]).
-export([get_protocol_options/1]).
-export([count_connections/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TAB, ?MODULE).

-type monitors() :: [{{reference(), pid()}, any()}].
-record(state, {
	monitors = [] :: monitors()
}).

%% API.

start_server()
	->	Server_Name = {local, ranch_server},
		Callback_Module = ranch_server,
		Init_Arg = [],
		Init_Option = [],
		{ok, Server_PID} = gen_server:start_link(Server_Name, Callback_Module, Init_Arg, Init_Option),
		{ok, Server_PID}.
%% -spec set_new_listener_opts(ranch:ref(), ranch:max_conns(), any()) -> ok.
set_new_listener_opts(Ref, MaxConns, Opts) ->
	Server_Ref = ranch_server,
	Args = {Ref, MaxConns, Opts},
	Request = [set_new_listener_opts, Args],
	Reply = gen_server:call(Server_Ref, Request),
	Reply.
'_on_set_new_listener_opts'(Args, State) ->
		[{Ref, MaxConns, Opts}] = Args,
		ets:insert(?TAB, {{max_conns, Ref}, MaxConns}),
		ets:insert(?TAB, {{opts, Ref}, Opts}),
		{reply, ok, State}.
%% -spec set_connections_sup(ranch:ref(), pid()) -> ok.
set_connections_sup(Ref, Pid) ->
	Server_Ref = ranch_server,
	Args = {Ref, Pid},
	Request = [set_connections_sup, Args],
	true = gen_server:call(Server_Ref, Request),
	ok.
'_on_set_connections_sup'(Args, State=#state{monitors=Monitors}) ->
		[{Ref, Pid}] = Args,
		case ets:insert_new(?TAB, {{conns_sup, Ref}, Pid}) of
			true ->
				MonitorRef = erlang:monitor(process, Pid),
				{reply, true, State#state{monitors=[{{MonitorRef, Pid}, Ref}|Monitors]}};
			false -> {reply, false, State}
		end.

%% -spec set_max_connections(ranch:ref(), ranch:max_conns()) -> ok.
set_max_connections(Ref, MaxConnections) ->
	Server_Ref = ranch_server,
	Args = {Ref, MaxConnections},
	Request = [set_max_conns, Args],
	Reply = gen_server:call(Server_Ref, Request),
	Reply.
'_on_set_max_conns'(Args, State) ->
		[{Ref, MaxConns}] = Args,
		ets:insert(?TAB, {{max_conns, Ref}, MaxConns}),
		ConnsSup = get_connections_sup(Ref),
		ConnsSup ! {set_max_conns, MaxConns},
		{reply, ok, State}.
%% -spec set_protocol_options(ranch:ref(), any()) -> ok.
set_protocol_options(Ref, ProtoOpts) ->
	Server_Ref = ranch_server,
	Args = {Ref, ProtoOpts},
	Request = [set_opts, Args],
	Reply = gen_server:call(Server_Ref, Request),
	Reply.
'_on_set_opts'(Args, State) ->
		[{Ref, Opts}] = Args,
		ets:insert(?TAB, {{opts, Ref}, Opts}),
		ConnsSup = get_connections_sup(Ref),
		ConnsSup ! {set_opts, Opts},
		{reply, ok, State}.
%% -spec cleanup_listener_opts(ranch:ref()) -> ok.
cleanup_listener_opts(Ref) ->
	Table_Name = ranch_server,
	_ = ets:delete(Table_Name, {addr, Ref}),
	_ = ets:delete(Table_Name, {max_conns, Ref}),
	_ = ets:delete(Table_Name, {opts, Ref}),
	%% We also remove the pid of the connections supervisor.
	%% Depending on the timing, it might already have been deleted
	%% when we handled the monitor DOWN message. However, in some
	%% cases when calling stop_listener followed by get_connections_sup,
	%% we could end up with the pid still being returned, when we
	%% expected a crash (because the listener was stopped).
	%% Deleting it explictly here removes any possible confusion.
	_ = ets:delete(Table_Name, {conns_sup, Ref}),
	ok.
%% -spec set_addr(ranch:ref(), {inet:ip_address(), inet:port_number()}) -> ok.
set_addr(Ref, Addr) ->
	Server_Ref = ranch_server,
	Args = {Ref, Addr},
	Request = [set_addr, Args],
	Reply = gen_server:call(Server_Ref, Request),
	Reply.
'_on_set_addr'(Args, State) ->
	[{Ref, Addr}] = Args,
	true = ets:insert(?TAB, {{addr, Ref}, Addr}),
	{reply, ok, State}.
%% -spec get_connections_sup(ranch:ref()) -> pid().
get_connections_sup(Ref) ->
	Table_Name = ranch_server,
	ets:lookup_element(Table_Name, {conns_sup, Ref}, 2).

%% -spec get_addr(ranch:ref()) -> {inet:ip_address(), inet:port_number()}.
get_addr(Ref) ->
	Table_Name = ranch_server,
	ets:lookup_element(Table_Name, {addr, Ref}, 2).

%% -spec get_max_connections(ranch:ref()) -> ranch:max_conns().
get_max_connections(Ref) ->
	Table_Name = ranch_server,
	ets:lookup_element(Table_Name, {max_conns, Ref}, 2).

%% -spec get_protocol_options(ranch:ref()) -> any().
get_protocol_options(Ref) ->
	Table_Name = ranch_server,
	ets:lookup_element(Table_Name, {opts, Ref}, 2).
%% -spec count_connections(ranch:ref()) -> non_neg_integer().
count_connections(Ref) ->
	ranch_conns_sup:active_connections(get_connections_sup(Ref)).



%% gen_server.
%% ranch_sup启动时会调用这个函数来初始化ranch_server进程
init(_Arg = []) ->
	Result_List = '_select'(),
	Monitors = ['_new_monitor'(Pid, Ref) || [Ref, Pid] <- Result_List],
	{ok, #state{monitors=Monitors}}.
'_select'() ->
	Table_Name = ranch_server,
	Ref_placeholder = '$1',
	Pid_placeholder = '$2',
	Match_Patern = {{conns_sup, Ref_placeholder}, Pid_placeholder},
	Result_List = ets:match(Table_Name, Match_Patern),
	Result_List.
% erlang:monitor(Type, Monitored_ID)在当前进程和被监控进程之间创建一个监控器，当被监控的进程关掉时，监控器就会关掉，并且当前进程才会得到消息
% 第一个参数表示被监控的类型 process | port | time_offset
% 第二个参数表示被监控实体的ID，可以是pid()，也可以是注册名
% 返回监控器的引用
'_new_monitor'(Monitored_Pid, Ref) ->
	MonitorRef = erlang:monitor(process, Monitored_Pid),
	{{MonitorRef, Monitored_Pid}, Ref}.

handle_call(Request, _From, State) ->
	[Func|Args] = Request,
	case Func of
		set_new_listener_opts -> '_on_set_new_listener_opts'(Args, State);
		set_connections_sup ->'_on_set_connections_sup'(Args, State);
		set_max_conns -> '_on_set_max_conns'(Args, State);
		set_addr -> '_on_set_addr'(Args, State);
		set_opts -> '_on_set_opts'(Args, State);
		_ -> {reply, ignore, State}
	end.
%% 
handle_cast(_Request, State) -> {noreply, State}.
%%
handle_info({'DOWN', MonitorRef, process, Pid, _}, State=#state{monitors=Monitors}) ->
	{_, Ref} = lists:keyfind({MonitorRef, Pid}, 1, Monitors),
	_ = ets:delete(?TAB, {conns_sup, Ref}),
	Monitors2 = lists:keydelete({MonitorRef, Pid}, 1, Monitors),
	{noreply, State#state{monitors=Monitors2}};
handle_info(_Info, State) -> {noreply, State}.
%% 
terminate(_Reason, _State) -> ok.
%%
code_change(_OldVsn, State, _Extra) -> {ok, State}.
