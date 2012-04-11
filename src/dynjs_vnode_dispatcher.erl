-module(dynjs_vnode_dispatcher).
-behaviour(gen_server2).

-include("dynjs.hrl").

-export([
  dispatch/1
]).
-export([
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, { config }).

start_link() ->
  gen_server2:start_link({local, ?MODULE}, ?MODULE, [], [{timeout, infinity}]).

init([]) ->  
  Config = application:get_all_env(dynjs),
  case application:get_env(dynjs, master_node) of
    {ok, MN} when MN =/= node() -> riak_core:join(MN);
    _ -> ok
  end,
  
  {ok, #state{ config=Config }}.

handle_call(Msg, From, State) ->
  io:format("handle_call: ~p ~p ~p~n", [Msg, From, State]),
  {noreply, State}.

handle_cast(Msg, State) ->
  io:format("handle_cast: ~p ~p~n", [Msg, State]),
  {noreply, State}.

handle_info(Msg, State) ->
  io:format("handle_info: ~p ~p~n", [Msg, State]),

  Hash = riak_core_util:chash_key({term_to_binary(now()), term_to_binary(now())}),
  Index = case riak_core_apl:get_primary_apl(Hash, 1, dynjs) of
    [{Idx, _Type}] -> Idx;
    _ -> {0, node()}
  end,
  lager:debug("Dispatching to ~p", [Index]),

  riak_core_vnode_master:command(Index, check_file_mod_time, dynjs_vnode_master),

  {noreply, State}.

terminate(Reason, State) ->
  io:format("terminate: ~p ~p~n", [Reason, State]),
  ok.

code_change(OldVsn, State, Extra) ->
  io:format("code_change: ~p ~p ~p~n", [OldVsn, State, Extra]),
  {ok, State}.

dispatch(Ctx) ->
  Hash = riak_core_util:chash_key({term_to_binary(now()), term_to_binary(now())}),
  Index = case riak_core_apl:get_primary_apl(Hash, 1, dynjs) of
    [{Idx, _Type}] -> Idx;
    _ -> {0, node()}
  end,
  lager:debug("Dispatching to ~p", [Index]),

  riak_core_vnode_master:command(Index, Ctx, dynjs_vnode_master),

  ok.
  
mkid(Script) ->
  % Absconded from riak_core_util:mkclientid/1
  {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
  {_,_,NowPart} = now(),
  Id = erlang:phash2([Y,Mo,D,H,Mi,S,Script,NowPart]),
  io_lib:format("~p", [Id]).
