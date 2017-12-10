-module(ranch_acceptor).

-export([start_link/3]).
-export([loop/3]).

%% -spec start_link(inet:socket(), module(), pid()) -> {ok, pid()}.
start_link(LSocket, Transport, ConnsSup)
	->	Pid = spawn_link(?MODULE, loop, [LSocket, Transport, ConnsSup]),%% ?MODULE宏会被展开成当前模块的名字
		{ok, Pid}.
%% -spec loop(inet:socket(), module(), pid()) -> no_return().
loop(LSocket, Transport, ConnsSup)
	-> 	_ = case Transport:accept(LSocket, infinity) of
				{ok, CSocket} ->
					case Transport:controlling_process(CSocket, ConnsSup) of
						ok ->
							%% This call will not return until process has been started
							%% AND we are below the maximum number of connections.
							ranch_conns_sup:start_protocol(ConnsSup, CSocket);
						{error, _} ->
							Transport:close(CSocket)
					end;
				%% Reduce the accept rate if we run out of file descriptors.
				%% We can't accept anymore anyway, so we might as well wait
				%% a little for the situation to resolve itself.
				{error, emfile} ->
					error_logger:warning_msg("Ranch acceptor reducing accept rate: out of file descriptors~n"),
					receive after 100 -> ok end;
					%% We want to crash if the listening socket got closed.
				{error, Reason} when Reason =/= closed -> ok
			end,
		flush(),
		?MODULE:loop(LSocket, Transport, ConnsSup).

flush()
	-> 	receive Msg ->
			error_logger:error_msg(
				"Ranch acceptor received unexpected message: ~p~n",
				[Msg]),
				flush()
		after 0 -> ok
		end.
