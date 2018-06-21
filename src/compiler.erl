-module(compiler).
-include("token.hrl").
-export([
  compile/1
	,get_variables/1
	,token_to_list/1
]).

compile(Data) ->
  Tokens = tokenizer:tokenize(Data),
  case parser:parse(Tokens) of
	  ok -> 
		  postfixer:postfix(Tokens);
		fail ->
		  throw({sheet,compiler,"Parse error"})
	end.	

get_variables([]) -> [];
get_variables([{?VARIABLE,Variable}|Rest]) ->
  Variables = get_variables(Rest),
	case lists:member(Variable,Variables) of
	  true -> Variables;
		false -> [Variable|Variables]
	end;	

get_variables([{?LIST,{MinCol,Row},{MaxCol,Row}}|Rest]) ->
  Variables = get_variables(Rest),
  [MinColInt] = MinCol,
  [MaxColInt] = MaxCol,
	Cols = lists:seq(MinColInt,MaxColInt),
	RowAsList = integer_to_list(Row),
	List = [lists:append([C],RowAsList) || C <- Cols],
	Atoms = [list_to_atom(X) || X <- List],
	Set = [ A || A <- Atoms,false==lists:member(A,Variables)],
	lists:append(Set,Variables);

get_variables([{?LIST,{Col,MinRow},{Col,MaxRow}}|Rest]) ->
  Variables = get_variables(Rest),
	Rows = lists:seq(MinRow,MaxRow),
	List = [lists:append(Col,integer_to_list(R)) || R <- Rows],
	Atoms = [list_to_atom(X) || X <- List],
	Set = [ A || A <- Atoms,false==lists:member(A,Variables)],
	lists:append(Set,Variables);

get_variables([_|Rest]) ->
  get_variables(Rest).

token_to_list({?NUMBER,I})  when is_integer(I) -> integer_to_list(I);
token_to_list({?NUMBER,I})  when is_float(I) -> float_to_list(I);
token_to_list({?STRING,S}) -> S.
% Code wraps a string in quotes.
% S1 = string:concat("\"",S),
% string:concat(S1,"\"").
