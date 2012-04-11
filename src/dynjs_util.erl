-module(dynjs_util).

-include_lib("erlv8/include/erlv8.hrl").

-export([
  read/2,
  run/3,
  resolve_module_path/2,
  resolve_filename/2,
  find_exports/3
]).

read(From, Data) ->
  %lager:info("reading from ~p with ~p", [From, Data]),
  case file:read(From, 16384) of
    {ok, D} -> read(From, Data ++ D);
    eof -> {ok, Data};
    Else -> Else
  end.
  
run(Filename, JsCtx, VM) ->
  case file:open(Filename, [read]) of
    {ok, F} ->
      %lager:info("opened file: ~p", [Filename]),
      case read(F, []) of
        {ok, JS} -> 
          %lager:info("Running ~p", [JS]),
          Result = erlv8_vm:run(VM, JsCtx, JS, {Filename, 0, 0}),
          %lager:info("got result: ~p", [Result]),
          Result;
        Else -> Else
      end;
    Else -> Else
  end.

resolve_module_path(Path, Mod) ->
  case string:tokens(Mod, "/") of
    [M] -> 
      Try1 = M ++ ".js",
      case resolve_filename(Path, Try1) of
        undefined -> 
          Try2 = filename:join(M, M ++ ".js"),
          case resolve_filename(Path, Try2) of
            undefined -> undefined;
            Filename -> Filename
          end;
        Filename -> Filename
      end;
    [_ | M] -> 
      Try1 = Mod ++ ".js",
      case resolve_filename(Path, Try1) of
        undefined -> 
          Try2 = filename:join(Mod, M ++ ".js"),
          case resolve_filename(Path, Try2) of
            undefined -> undefined;
            Filename -> Filename
          end;
        Filename -> Filename
      end
  end.

resolve_filename(Path, RelativePath) ->
  case file:path_open(Path, RelativePath, [read]) of
    {ok, F, Name} ->
      file:close(F),
      Name;
    _ -> undefined
  end.

find_exports(File, JsCtx, VM) ->
  case File of
    undefined -> [];
    _ ->
      lager:info("finding exports in file: ~p", [File]),
      case run(File, JsCtx, VM) of
        {ok, _} -> 
          case erlv8_vm:run(VM, JsCtx, "exports") of
            {ok, Ex} -> Ex:proplist();
            Else -> 
              lager:info("exports=~p", [Else]),
              []
          end;
        {throw, {erlv8_object, _, _}} ->
          throw("Error evaluating file " ++ File);
        Else -> 
          lager:info("no export object ~p", [Else]),
          []
      end
  end.