-module(ranch_sup).
-behaviour(supervisor).
% API
-export([start_link/0]).
% Callback
-export([init/1]).

%%-spec start_link() -> {ok, pid()}.
% supervisor:start_link(SupName::sup_name(), Module::module(), Args::term()) -> {ok, pid()} | ignore | {error, startlink_err()}
% sup_name() = {local, Name :: atom()} |
%              {global, Name :: atom()} |
%              {via, Module :: module(), Name :: any()}
% 创建一个监督者，作为监督树的一部分，例如这个函数确保监督者被链接到调用进程(即这个监督者的监督者)
% 被创建的监督者进程会调用Module:init/1来弄清楚重启策略，最大重启次数和子进程
% 为了保证同步启动程序，这个函数在所有子进程都启动并且Module:init/1返回后才会返回
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
% start_link() -> supervisor:start_link({local, ranch_sup}, ranch_sup, []).

init(_Args=[]) ->
	% ets:new(Table_Name, [Option]) -> Table_Name
	% Table_Name = atom()
	ranch_server = ets:new(ranch_server, [ordered_set, public, named_table]),
	SupFlags = #{
		strategy => one_for_one,
		intensity => 1,
		period => 5
	},
	ChildSpec_1 = #{
		id => ranch_server,
		start => {ranch_server, start_link, []},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [ranch_server]
	},
	ChildSpecs = [ChildSpec_1],
	%%Procs = [ { ranch_server, 
	%%			{ranch_server, start_link, []},
	%%			permanent, 
	%%			5000, 
	%%			worker, 
	%%			[ranch_server]
	%%		}],
	%{ok, {{one_for_one, 1, 5}, Procs}}.
	{ok, {SupFlags, ChildSpecs}}.
