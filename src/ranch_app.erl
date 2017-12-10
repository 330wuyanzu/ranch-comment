-module(ranch_app).
-behaviour(application).
%% Callback
-export([start/2]).
-export([stop/1]).
%-export([profile_output/0]).

start(_, _) 
	->%	_ = consider_profiling(),
		ranch_sup:create_supervisor().

stop(_) -> ok.
% 根据.app的配置参数来决定是否对应用进行性能分析
%consider_profiling() 
%	->	case application:get_env(profile) of
%			{ok, true} -> {ok, _Pid} = eprof:start(),
%						  eprof:start_profiling([self()]);
%			_ -> not_profiling
%		end.
		% application:get_env/1
% application:get_env(Para) -> undefined | {ok, Value}
% 获取应用的配置参数Para的值
% 如果该应用没有加载，或者
% 配置参数不存在，或者
% 执行这个调用的进程不属于任何一个应用
% 会返回undefined

% eprof:start/0
% eprof:start() -> {ok, PID} | {error, Reason}
% 启动Eprof服务器，用来存储收集到的数据的内部状态

% eprof:start_profiling/1
% eprof:start_profiling([atom() | pid()])
% 开始分析进程

%% -spec profile_output() -> ok.
%% eprof是一个Erlang的时间分析工具
%profile_output() 
%->	eprof:stop_profiling(),
%	eprof:log("procs.profile"),
%	eprof:analyze(procs),
%	eprof:log("total.profile"),
%	eprof:analyze(total).
