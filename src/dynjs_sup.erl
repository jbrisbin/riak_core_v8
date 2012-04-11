-module(dynjs_sup).
-behaviour(supervisor).

-include("dynjs.hrl").

-export([
  start_link/0,
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  Workers = [
    ?GEN_SERVER(dynjs_vnode_dispatcher_master, dynjs_vnode_dispatcher),
    ?VNODE_MASTER(dynjs_vnode_master, dynjs_vnode)
  ],
  
  {ok, {{one_for_one, 10, 10}, Workers}}.