-module(dynjs).

-export([
  start/0,
  stop/0
]).

start() ->
  application:start(crypto),
  application:start(sasl),
  application:start(lager),
  application:start(webmachine),
  application:start(os_mon),
  application:start(riak_sysmon),
  application:start(riak_core),
  application:start(erlv8),
  application:start(dynjs).

stop() ->
  application:stop(dynjs),
  application:stop(erlv8),
  application:stop(riak_core),
  application:stop(webmachine),
  ok.
