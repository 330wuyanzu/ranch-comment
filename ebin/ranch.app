%% {application, ApplicationName, [Opt1,...,OptN]}.
%% ApplicationName是一个原子，表示应用的名字，
%% 并且应用资源文件必须是ApplicationName.app
%% 每个Opt都是一个元组{Key, Value}，用来定义应用的某些属性
%% mod键定义应用的回调模块和启动参数
%% modules键定义应用引用的所有模块
%% registered键定义应用中所有注册进程的名字
%% applications键定义所有在应用启动之前必须启动的其他应用
{
	application, 
	'ranch', 
	[
		{description, "Socket acceptor pool for TCP protocols."},
		{vsn, "1.4.0"},
		{modules, [
			'ranch',
			'ranch_acceptor',
			'ranch_acceptors_sup',
			'ranch_app',
			'ranch_conns_sup',
			'ranch_listener_sup',
			'ranch_protocol',
			'ranch_server',
			'ranch_ssl',
			'ranch_sup',
			'ranch_tcp',
			'ranch_transport'
		]},
		{registered, [ranch_sup,ranch_server]},
		{applications, [kernel, stdlib, ssl]},
		{mod, {ranch_app, []}},
		{env, []}
	]
}.