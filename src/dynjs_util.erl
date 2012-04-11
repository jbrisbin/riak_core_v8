-module(dynjs_util).

-include_lib("erlv8/include/erlv8.hrl").

-export([
  read/2,
  run/3,
  resolve_module_path/2,
  resolve_filename/2,
  find_exports/3,
  convert/1,
  erlang_apply/1
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
      %lager:info("finding exports in file: ~p", [File]),
      case run(File, JsCtx, VM) of
        {ok, _} -> 
          case erlv8_vm:run(VM, JsCtx, "exports") of
            {ok, Ex} -> Ex:proplist();
            _Else -> 
              %lager:info("exports=~p", [Else]),
              []
          end;
        {throw, {erlv8_object, _, _}} ->
          throw("Error evaluating file " ++ File);
        _Else -> 
          %lager:info("no export object ~p", [Else]),
          []
      end
  end.

convert(Obj) ->
  %lager:info("trying to convert: ~p", [Obj]),
  case Obj of
    {erlv8_array, _, _}=A -> 
      %lager:info("array=~p", [A:list()]),
      lists:map(fun convert/1, A:list());
    {erlv8_fun, _, _}=F -> 
      lists:map(fun convert/1, (F:object()):proplist());
    {erlv8_object, _, _}=O -> 
      lists:map(fun convert/1, O:proplist());
    {O1, O2} -> {convert(O1), convert(O2)};
    O when is_list(O) -> 
      lists:map(fun convert/1, O);
    O -> 
      %lager:info("other=~p", [O]),
      O
  end.
  
erlang_apply(Args) ->
  %lager:info("converting args: ~p", [Args]),
  ConvertedArgs = lists:map(fun convert/1, Args),
  [M, F | A] = ConvertedArgs,
  Ma = erlang:binary_to_atom(M, utf8),
  Fa = erlang:binary_to_atom(F, utf8),

  erlang:apply(Ma, Fa, A).
  