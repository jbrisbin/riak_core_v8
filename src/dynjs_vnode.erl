-module(dynjs_vnode).
-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").
-include_lib("erlv8/include/erlv8.hrl").

-export([
  delete/1,
  encode_handoff_item/2,
  handle_info/2,
  handle_command/3,
  handle_coverage/4,
  handle_exit/3,
  handle_handoff_command/3,
  handle_handoff_data/2,
  handoff_cancelled/1,
  handoff_finished/2,
  handoff_starting/2,
  init/1,
  is_empty/1,
  start_vnode/1,
  terminate/2
]).

-record(state, {
  partition :: partition(),
  vm :: pid(), 
  config :: term(),
  path,
  mod,
  mod_file,
  last_mod_table,
  exports,
  refresh_interval
}).

start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
  Config = application:get_all_env(dynjs),
  Path = case lists:keyfind(lib_dirs, 1, Config) of
    false -> [];
    {lib_dirs, Ds} -> Ds
  end,
  HandlerMod = case lists:keyfind(handler, 1, Config) of
    false -> "null";
    {handler, H} -> H
  end,
  ModFile = dynjs_util:resolve_module_path(Path, HandlerMod),
  RefreshInterval = case lists:keyfind(refresh_interval, 1, Config) of
    false -> 5000;
    {refresh_interval, R} -> R
  end,

  %lager:info("Starting dynjs VM ~p", [Partition]),
  {ok, VM} = erlv8_vm:start(),
  Global = erlv8_vm:global(VM),
  Global:set_value("exports", ?V8Obj([])),
  Global:set_value("log", ?V8Obj([
    {"debug", fun(_Inv, [LogMsg]) -> mod_log(debug, binary_to_list(LogMsg), []);
                 (_Inv, [FmtSpec | Args]) -> mod_log(debug, binary_to_list(FmtSpec), dynjs_util:convert(Args))
              end},
    {"info", fun(_Inv, [LogMsg]) -> mod_log(info, binary_to_list(LogMsg), []);
                (_Inv, [FmtSpec | Args]) -> mod_log(info, binary_to_list(FmtSpec), dynjs_util:convert(Args))
             end},
    {"error", fun(_Inv, [LogMsg]) -> mod_log(error, binary_to_list(LogMsg), []);
                 (_Inv, [FmtSpec | Args]) -> mod_log(error, binary_to_list(FmtSpec), dynjs_util:convert(Args))
              end}
  ])),
  Global:set_value("require", fun(_Inv, [M]) ->   
    %lager:info("requiring module: ~p", [M]),

    ReqCtx = erlv8_context:new(VM),
    (erlv8_context:global(ReqCtx)):set_value("exports", ?V8Obj([])),
    ReqModPath = dynjs_util:resolve_module_path(Path, binary_to_list(M)),
    ReqExports = dynjs_util:find_exports(ReqModPath, ReqCtx, VM),
    %lager:info("setting exports from ~p to ~p", [M, ReqExports]),
    
    ?V8Obj(ReqExports)
  end),
  Global:set_value("erlang", ?V8Obj([
    {"apply", fun(_, Args) -> dynjs_util:erlang_apply(Args) end}
  ])),

  Exports = dynjs_util:find_exports(ModFile, erlv8_context:get(VM), VM),
  %lager:info("dynjs exports: ~p", [Exports]),

  LastModTable = ets:new(last_modified, []),

  erlang:send_after(RefreshInterval, self(), refresh),
  
  {ok, #state { partition=Partition, 
                vm=VM, 
                config=Config, 
                path=Path, 
                mod=HandlerMod,
                mod_file=ModFile,
                exports=Exports,
                last_mod_table=LastModTable,
                refresh_interval=RefreshInterval }}.

handle_info(refresh, #state{ vm=VM, 
                             mod_file=Filename, 
                             last_mod_table=LastModTable, 
                             refresh_interval=RefreshInterval }=State) ->
  LastMod = filelib:last_modified(Filename),
  NewState = case ets:lookup(LastModTable, Filename) of
    [] -> 
      ets:insert(LastModTable, {Filename, LastMod}),
      State;
    [{Filename, LastModified}] when LastMod > LastModified ->
      Exports = dynjs_util:find_exports(Filename, erlv8_context:get(VM), VM),
      %lager:info("found exports: ~p", [Exports]),
      ets:insert(LastModTable, {Filename, LastMod}),
      State#state{ exports=Exports };
    [{Filename, _LastModified}] -> 
      %lager:info("~p > ~p", [LastMod, LastModified]),
      State
  end,
  erlang:send_after(RefreshInterval, self(), refresh),
  {ok, NewState};
  
handle_info(_, State) ->
  {ok, State}.
  
handle_command(Msg, _Sender, #state{ vm=_VM, 
                                     config=_Config, 
                                     path=_Path, 
                                     exports=Exports }=State) ->
  lager:info("dispatch to VM: ~p~n", [?V8Obj(Msg)]),
  
  case invoke_handler(<<"handle_command">>, Exports, Msg) of
    noreply -> 
      {noreply, State};
    Obj -> 
      %lager:info("reply with: ~p", [Obj]),
      {reply, {ok, Obj}, State}
  end.

handle_coverage(Request, KeySpaces, Sender, State) ->
  lager:debug("handle_coverage: ~p ~p ~p ~p~n", [Request, KeySpaces, Sender, State]),
  {continue, State}.
  
handle_exit(Pid, Reason, #state{ vm=VM }=State) ->
  erlv8_vm:stop(VM),
  lager:debug("handle exit: ~p ~p~n", [Pid, Reason]),
  {stop, Reason, State}.

handle_handoff_command(Message, Sender, State) ->
  lager:debug("handle handoff: ~p ~p~n", [Message, Sender]),
  {forward, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

handle_handoff_data(_Data, State) ->
  {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
  <<>>.

is_empty(State) ->
  {true, State}.

delete(State) ->
  {ok, State}.

terminate(_Reason, #state{ last_mod_table=LastModTable }=_State) ->
  ets:delete(LastModTable),
  ok.

mod_log(Level, [], Args) ->
  Ps = lists:map(fun(_) -> "~p" end, Args),
  FmtSpec = string:join(Ps, " "),
  mod_log(Level, FmtSpec, Args);
  
mod_log(Level, FmtSpec, Args) ->
  case Level of
    debug -> lager:debug(FmtSpec, Args);
    info -> lager:info(FmtSpec, Args);
    error -> lager:error(FmtSpec, Args)
  end.

invoke_handler(Handler, Exports, Arg) ->
  case lists:keyfind(Handler, 1, Exports) of
    {Handler, {erlv8_fun, _, _}=F} ->
      case F:call(?V8Obj(Arg)) of
        {throw, {error, ErrorObj}}=E -> 
          lager:error("Error invoking handler ~s ~p", [Handler, ErrorObj:proplist()]),
          E:proplist();
        undefined -> 
          noreply;
        Obj -> 
          dynjs_util:convert(Obj)
      end;
    _ -> noreply
  end.
