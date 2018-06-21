-module(sheet_server).
-export([
  start/0
  ,loop/1
  ,init/0
  ,set/3
  ,get/2
  ,calc/1
  ,stop/1
]).
start() ->
  spawn(sheet_server,init,[]).

stop(Pid) ->
  Pid!{self(),stop},
	receive
	  {Pid,Q} -> Q
	end.

init() ->
  %io:format("sheet_server - starting~n",[]),
  loop(sheet:new()).

set(Pid,Cell,Value) ->
  Pid!{self(),set,Cell,Value},
	receive
	  {Pid,Q} -> Q
	end.

get(Pid,Cell) ->
  Pid!{self(),get,Cell},
	receive
	  {Pid,Q,Value} -> {Q,Value}
	end.

calc(Pid) ->
  Pid!{self(),calc},
	receive
	  {Pid,Q} -> Q
	end.

loop(Sheet) ->
  receive
	  % set a cell to a formula
    {Pid,set,Cell,Value} when is_list(Value) ->
		  Sheet1 = sheet:set(Sheet,Cell,Value),
      Pid!{self(),ok},
      loop(Sheet1);
    {Pid,set,_,_} ->
      Pid!{self(),nok};
		% get an evaluated cell	
    {Pid,get,Cell} ->
		  {Sheet1,Value} = sheet:get(Sheet,Cell),
      Pid!{self(),ok,Value},
      loop(Sheet1);
    {Pid,stop} ->
      Pid!{self(),ok},
      true;
    X ->
		  io:format("sheet_server - unexpected message: ~w~n",[X]),
      loop(Sheet)
  end.  
