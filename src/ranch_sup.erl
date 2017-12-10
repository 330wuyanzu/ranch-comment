-module(ranch_sup).
-behaviour(supervisor).
% API
-export([create_supervisor/0]).
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
create_supervisor() 
	-> 	Type = local,
		Name = ranch_sup,
		SupName = {Type, Name},
		Callback_Module = ranch_sup,
		Init_Arg = [],
		supervisor:start_link(SupName, Callback_Module, Init_Arg).

init(_Args=[]) ->
	% ets:new(Table_Name, [Option]) -> Table_Name
	% Table_Name = atom()
	Table_Name = ranch_server,
	Option_List = [ordered_set, public, named_table],
	Table_Name = ets:new(Table_Name, Option_List),
	Supervisor_Flags = #{
		strategy => one_for_one,
		intensity => 1,
		period => 5
	},
	Child_Spec_1 = #{
		id => ranch_server,
		start => {ranch_server, start_server, []},% {M, F, A}
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [ranch_server]
	},
	Child_Spec_List = [Child_Spec_1],
    % 监督者被创建后，会顺序遍历Child_Spec_List，根据Supervisor_Flags创建子进程，
	% 监督者被关闭时，会倒序遍历Child_Spec_List，关闭子进程。
	{ok, {Supervisor_Flags, Child_Spec_List}}.
