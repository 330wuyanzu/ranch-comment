# 流程

- ranch_app:start()启动应用，其中调用ranch_sup:start_link()启动监督树
- ranch_sup:start_link()启动监督树，监督树调用ranch_sup:init()启动初始化ranch_server
- ranch_server实现gen_server行为