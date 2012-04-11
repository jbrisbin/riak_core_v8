-module(dynjs_app).
-behaviour(application).

-export([
  start/2, 
  stop/1
]).

start(_StartType, _StartArgs) ->
  case dynjs_sup:start_link() of
    {ok, Pid} ->
      ok = riak_core:register([{vnode_module, dynjs_vnode}]),
      ok = riak_core_node_watcher:service_up(dynjs, self()),
      
      {ok, Pid};
    Else -> Else
  end.

stop(_State) ->
  ok.
