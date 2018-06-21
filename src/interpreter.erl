-module(interpreter).
-include("token.hrl").
-export([
	run/1
	,resolve/2
	,run/2
]).
run(L) ->
  {_,X} = run1(L,[],sheet:new()),
	X.
  
run(L,Sheet) ->
  run1(L,[],Sheet).

run1([],[X],Sheet) -> 
  resolve(X,Sheet);

run1([],_,_) -> throw({sheet,interpreter,"Invalid Expression."});

run1([{?FUNCTION,Functor}|RestOfPostfix],Stack,Sheet) ->
	{Sheet1,Stack1} = invoke(Functor,Sheet,Stack),
	run1(RestOfPostfix,Stack1,Sheet1);

run1([{?OPERATOR,Oper}|RestOfPostfix],[X,Y|Stack],Sheet) ->
  {Sheet1,{_,XR}} = resolve(X,Sheet),
	{Sheet2,{_,YR}} = resolve(Y,Sheet1),
  case Oper of
	  plus -> Z = YR + XR;
	  minus -> Z = YR - XR;
	  times -> Z = YR * XR;
	  divide -> Z = YR / XR;
	  less_than -> Z = relational(YR < XR); 
	  greater_than -> Z = relational(YR > XR);  
	  greater_than_or_equal -> Z = relational(YR >= XR);  
	  less_than_or_equal -> Z = relational(YR =< XR);  
	  equality -> Z = relational(YR == XR);  
	  not_equal -> Z = relational(YR /= XR)   
	end,
	run1(RestOfPostfix,[{?NUMBER,Z}|Stack],Sheet2);

run1([Token|RestOfPostfix],Stack,Sheet) ->
  run1(RestOfPostfix,[Token|Stack],Sheet).

relational(true) -> 1;
relational(false) -> 0.

invoke(Functor,Sheet,Stack) ->
  {Arity,_Types,ReturnType} = functions:properties(Functor),
  {Sheet1,Stack1,List} = marshall_function_parms(Arity,Sheet,Stack),
  Result = functions:dispatch(Functor,List),
	{Sheet1,[{ReturnType,Result}|Stack1]}.

marshall_function_parms(0,Sheet,Stack) -> {Sheet,Stack,[]};
marshall_function_parms(N,Sheet,Stack) ->
  {Sheet1,[Top|Stack1],List} = marshall_function_parms(N-1,Sheet,Stack),
  {Sheet2,ResolvedTop} = resolve(Top,Sheet1),
	{Sheet2,Stack1,[ResolvedTop|List]}.

resolve({?NUMBER,X},Sheet) ->
  {Sheet,{?NUMBER,X}};
resolve({?VARIABLE,V},Sheet) -> 
  sheet:get_(Sheet,V);
resolve({?STRING,S},Sheet) -> 
  {Sheet,{?STRING,S}};
resolve({?LIST,Min,Max},Sheet) ->
  {Sheet,{?LIST,resolve_list(Sheet,Min,Max)}}.

% When row and col are the same. then get the cell value and return it.
resolve_list(Sheet,{Row,Col},{Row,Col}) ->
  {_,Value} = sheet:get_(Sheet,form_variable(Row,Col)),
  [Value];

% Row the same column different.
resolve_list(Sheet,{Row,Col1},{Row,Col2}) when Col1 < Col2 ->
  List = resolve_list(Sheet,{Row,Col1+1},{Row,Col2}),
  {_,Value} = sheet:get_(Sheet,form_variable(Row,Col1)),
  [Value|List];
 
% Row different and column the same 
resolve_list(Sheet,{[Row1],Col},{[Row2],Col}) when Row1 < Row2 ->
  List = resolve_list(Sheet,{[Row1+1],Col},{[Row2],Col}),
  {_,Value} = sheet:get_(Sheet,form_variable([Row1],Col)),
  [Value|List].
  
form_variable(Row,Col) when is_list(Row) and is_integer(Col) ->
  list_to_atom(lists:append(Row,integer_to_list(Col))).
