-module(parser).
-include("token.hrl").
-export([
  parse/1
  ,normalize/1
]).

parse(L) -> 
  S = normalize(L),
  %io:format("Will reduce ~s~n",[S]),
  apply_rules(S).

apply_rules(S) ->
  Status = apply_each_rule(S,stop_condition(),rules()),
  case Status of
    ok -> ok;
    S1 -> 
      case S1 == S of
        true -> fail;
        false -> apply_rules(S1)
      end
  end.    

apply_each_rule(Stop,Stop,_) -> ok;
apply_each_rule(Data,_,[]) -> Data;
apply_each_rule(Data,Stop,[Rule|RestOfRules]) ->
  {Before,After} = Rule,
  {ok,NewData,C} = re:gsub(Data,Before,After),
  case C of
    0 -> apply_each_rule(NewData,Stop,RestOfRules);
    _ -> %io:format("reduced ~s to ~s~n",[Data,NewData]),
      NewData 
  end.  

normalize([]) ->[];
normalize([{?VARIABLE,_}|Rest]) ->
  append(t,normalize(Rest));
normalize([{?NUMBER,_}|Rest]) ->
  append(t,normalize(Rest));
normalize([{?OPERATOR,lparen}|Rest]) ->
  append("{",normalize(Rest));
normalize([{?OPERATOR,rparen}|Rest]) ->
  append("}",normalize(Rest));
normalize([{?OPERATOR,comma}|Rest]) ->
  append(",",normalize(Rest));
normalize([{?OPERATOR,_}|Rest]) ->
  append(o,normalize(Rest));
normalize([{?STRING,_}|Rest]) ->
  append(s,normalize(Rest));
normalize([{?LIST,_,_}|Rest]) ->
  append(l,normalize(Rest));
normalize([{?FUNCTION,_}|Rest]) ->
  append(f,normalize(Rest)).

append([H],List) ->
  [H|List];
append(A,List) ->
  [H|_] = atom_to_list(A),
  [H|List].

  stop_condition() -> "x".
  rules() ->[
    {"^t$","x"},    % a single terminal (number or variable)
    {"^s$","x"},    % a single string
    {"f{t}","t"},   % function with a single parm
    {"f{l}","t"},   % function with a list
    {"{t}","t"},    % terminal with parens
    {"tot","t"},    % terminal operator terminal
    {"f{t,t","f{t"} % function with multiple terms
    ].
