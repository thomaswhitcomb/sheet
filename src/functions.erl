-module(functions).
-include("token.hrl").
-export([
	dispatch/2
	,properties/1
]).

dispatch(square,[{?NUMBER,Parm}]) ->
  Parm*Parm;

dispatch(round,[{?NUMBER,Parm}]) ->
  round(Parm);

dispatch(trunc,[{?NUMBER,Parm}]) ->
  trunc(Parm);

dispatch(sin,[{?NUMBER,Parm}]) ->
  math:sin(Parm);

dispatch(sum,[{?LIST,List}]) ->
  compute_list_sum(List);

dispatch(avg,[{?LIST,List}]) ->
  X = compute_list_sum(List),
	X/length(List);

dispatch(ifthenelse,[{?NUMBER,If},{_,Then},{_,Else}]) ->
  %io:format("in ifthenelse.... with ~w~w~w~n",[If,Then,Else]),
	case If of
	  0 -> Else;
		_ -> Then
	end	;
dispatch(Functor,Parms) ->
  throw({interpreter,dispatch,io_lib:format("Function: ~w with ~w not defined",[Functor,Parms])}).

% Properties of a function: arity, input types and return type
properties(ifthenelse) -> {3,
  [[?NUMBER,?STRING],[?NUMBER,?STRING],[?NUMBER,?STRING]],
	?NUMBER};
properties(square) -> {1,[?NUMBER],?NUMBER};
properties(round) -> {1,[?NUMBER],?NUMBER};
properties(sin) -> {1,[?NUMBER],?NUMBER};
properties(avg) -> {1,[?LIST],?NUMBER};
properties(sum) -> {1,[?LIST],?NUMBER};
properties(trunc) -> {1,[?NUMBER],?NUMBER}.

compute_list_sum([]) ->0;
compute_list_sum([{?NUMBER,X}|Rest]) ->
  Y = compute_list_sum(Rest),
	X+Y.
