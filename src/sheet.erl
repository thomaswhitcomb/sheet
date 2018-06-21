-module(sheet).
-include("token.hrl").
-export([
  new/0
  ,clear/2
  ,set/3
  ,get/2
  ,get_formula/2
  ,get_/2
  ,calc/1
  ,cells/1
  ,check_circularity/2
  ,load/2
  ,load_properties/2
  ,carve_into_properties/1
]).
new() -> {dict:new(),dict:new()}. % cells and a cache

% clear/delete a cell
clear(Sheet,Cell) ->
  {Cells,Cache} = Sheet, 
	Cells1 = dict:erase(Cell,Cells),
	Cache1 = dict:erase(Cell,Cache),
	{Cells1,Cache1}.

% Set a cell to a string containing formula
set(Sheet,Cell,Formula) ->
  {Cells,_Cache} = Sheet, 
  Code = compiler:compile(Formula),
  Cells1 = dict:store(Cell,{Formula,Code},Cells),
	check_circularity(Cells1,Cell),
	{Cells1,dict:new()}.  % clear the cache

check_circularity(Cells,Cell) ->
	check_circularity1(Cells,[Cell],[]).
  
check_circularity1(_Cells,[],_) -> ok;
check_circularity1(Cells,[Var|RestOfVars],FoundSoFar) ->
  case lists:member(Var,FoundSoFar) of
	  true -> throw({sheet,circularity,io_lib:format("Circularity found with ~w",[Var])});
		_ -> ok
	end,

	case dict:find(Var,Cells) of
	  {ok,{_,Code}} ->
				NextVars = compiler:get_variables(Code),
				check_circularity1(Cells,NextVars,[Var|FoundSoFar]),
				check_circularity1(Cells,RestOfVars,FoundSoFar);
		_ -> 	
	      check_circularity1(Cells,RestOfVars,FoundSoFar)
	end.



% Get a cell's computed value as a string.
get_formula(Sheet,Cell) ->
  {Cells,_Cache} = Sheet, 
	Result = dict:find(Cell,Cells),
	case Result of
	  {ok,{Formula,_}} -> Formula;
	  _ -> ""
	end.

% Get a cell's computed value as a string.
get(Sheet,Cell) ->
  {Sheet1,Value} = get_(Sheet,Cell),
	{Sheet1,compiler:token_to_list(Value)}.

% Get a cell's computed value as a tuple {type,value}
get_(Sheet,Cell) ->
  {Cells,Cache} = Sheet,
	CellExists = dict:find(Cell,Cells),
	% See if requested shell is defined
  case CellExists of

	error -> % variable not defined 
	  {Sheet,{?NUMBER,0}};

	% variable exists in cells	
	{ok,CellHit} ->
	  InCache = dict:find(Cell,Cache),
    case InCache of

	  error -> % Not in cache
      {_Orig,Code} = CellHit,
	    {Sheet1,Value} = interpreter:run(Code,Sheet),
	    {Cells1,Cache1} = Sheet1,
		  % Save returned value in cache
	    Cache2 = dict:store(Cell,Value,Cache1),
	    {{Cells1,Cache2},Value};

		% Value in cache	
	  {ok,CacheHit} ->
		  %io:format("got a cache hit: ~w~n",[CacheHit]),
	    {Sheet,CacheHit}
		end
	end.

load(Sheet,Lines) when is_list(Lines) ->
  Lines1 = string:strip(Lines),
  L = length(Lines1),
  % put a newline (10) on the end if one does not exist
  case string:rchr(Lines1,10) of
	  L ->
		  Lines2 = Lines1;
		_ ->
		  Lines2 = lists:append(Lines1,[10])
	end,		
  Properties = carve_into_properties(Lines2),
	load_properties(Sheet,Properties)
	.

load_properties(Sheet,[]) -> Sheet;
load_properties(Sheet,[Property|Properties]) ->
  {Cell,Formula} = Property,
	Sheet1 = sheet:set(Sheet,Cell,Formula),
	load_properties(Sheet1,Properties).
  
carve_into_properties([]) -> [];
carve_into_properties(Lines) ->
  case re:first_match(Lines,"^.+\\n") of
	  {match,Start,Length} ->
		  NextLines = string:substr(Lines,Length+1),
			Line = string:substr(Lines,Start,Length-1),
			Property = line_to_property(Line),
			[Property|carve_into_properties(NextLines)];
		nomatch ->
		  throw({sheet,load,Lines})
	end.

line_to_property(Line) -> 
  % 61 is '='
	case string:chr(Line,61) of
	  0 -> throw({sheet,line_to_property,Line});
	  1 -> throw({sheet,line_to_property,Line});
	  N -> {list_to_atom(string:strip(string:substr(Line,1,N-1))),string:strip(string:substr(Line,N+1))}
	end.	
  

calc(Sheet) ->
  {Cells,_Cache} = Sheet,
	Keys = dict:fetch_keys(Cells),
	calc1(Sheet,Keys).

calc1(Sheet,[]) -> Sheet;
calc1(Sheet,[Cell|RestOfCells]) ->
  Sheet1 = calc1(Sheet,RestOfCells),
	{Sheet2,_} = sheet:get_(Sheet1,Cell),
	Sheet2.

% Return a list of {cell, cell contents}
cells(Sheet) ->
  {Cells,_Cache} = Sheet,
	Keys = dict:fetch_keys(Cells),
	cells1(Sheet,Keys).

cells1(_Sheet,[]) -> [];
cells1(Sheet,[Key|RestOfCells]) ->
  {Cells,_Cache} = Sheet,
  Value = dict:fetch(Key,Cells), 
	{Orig,_} = Value,
	{Sheet2,Result} = sheet:get(Sheet,Key),
	[{Key,Orig,Result}|cells1(Sheet2,RestOfCells)].
