-module(tokenizer).
-include("token.hrl").
-export([
  tokenize/1
  ,divide_cell_id/1
  ,validate_cell_id_structure/2
  ,token_to_list/1
]).

validate_cell_id_structure({A,B},{C,D}) when 
  (A == C) and (B =< D) and 
  (is_list(A)) and
  (length(A) == 1) -> ok;
validate_cell_id_structure({A,B},{C,D}) when
  (B == D) and (A =< C) and
  (is_list(A)) and
  (length(A) == 1) -> ok;
validate_cell_id_structure({A,B},{C,D}) -> throw({sheet,tokenizer,io_lib:format("Invalid range: ~w~w:~w~w" ,[A,B,C,D])}).

divide_cell_id(Id) ->
  {match,_S1,L1} = re:first_match(Id,"^[a-zA-Z]+"),
  Prefix = string:substr(Id,1,L1),
  Postfix = string:substr(Id,L1+1),
  {Prefix,list_to_integer(Postfix)}.

get_patterns_and_handlers() ->
  [
   {
     % Lists/ranges
     "^[a-zA-Z]+[0-9]+:[a-zA-Z]+[0-9]+",
     fun(Token,_Data) ->
       LcToken = convert_to_lower_case(Token),
       Index = string:str(LcToken,":"),
       Min = string:substr(LcToken,1,Index-1),
       Max = string:substr(LcToken,Index+1),
       MinPrefixPostfix = divide_cell_id(Min),
       MaxPrefixPostfix = divide_cell_id(Max),
       validate_cell_id_structure(MinPrefixPostfix,MaxPrefixPostfix),
       {?LIST,MinPrefixPostfix,MaxPrefixPostfix}
     end
   },
   {
     % alphanumerics (cell and variable identifiers)
     "^[a-zA-Z]+[0-9]*",
     fun(Token,Data) ->
       % see if the alpanumeric is followed by a lparen.
       % if true then assume it is a function call
       % otherwise it is a variable.
       LcToken = convert_to_lower_case(Token),
       case re:first_match(Data,"^\s*[\(]") of
         {match,_,_} ->
          {?FUNCTION,list_to_atom(LcToken)};
         nomatch ->
          {?VARIABLE,list_to_atom(LcToken)}
       end
     end   
   },
   {
     % Strings
     "^\".*\"",
     fun(Token,_Data) ->
       % remove outer quotes from string
       String = string:substr(Token,2,length(Token)-2),
       {?STRING,String}
     end   
   },
   {
     % Floats
     "^[-]?[0-9]+\\.[0-9]+",
     fun(Token,_Data) ->
       {Float,[]} = string:to_float(Token),
       {?NUMBER,Float}
     end
   },
   {
     % Integers
     "^[-]?[0-9]+",
     fun(Token,_Data) ->
       {Int,[]} = string:to_integer(Token),
       {?NUMBER,Int}
     end   
   },
   {
     % Dual operators
     "^==|^<=|^>=|^!=",
     fun(Token,_Data) ->
       {?OPERATOR,operator(Token)}
     end   
   }
 ].   

    
tokenize(Data) ->
  {ok,Data1,_} = re:gsub(Data," +"," "),  % reduce consectutive spaces to 1
  tokenize1(Data1).


tokenize1([]) -> [];
% Skip blanks
tokenize1([H|Rest]) when H == 32 -> 
  tokenize1(Rest);
% Parse Atoms and Variables
tokenize1(Data) -> 
  {Data1,Token} = determine_token(Data,get_patterns_and_handlers()),
  [Token|tokenize1(Data1)].

convert_to_lower_case([]) ->[];
convert_to_lower_case([H|T]) when (H >= 65) and (H =< 90) ->
  [H+32|convert_to_lower_case(T)];
convert_to_lower_case([H|T])  ->
  [H|convert_to_lower_case(T)].

determine_token(Data,[]) ->
  {string:substr(Data,2),{?OPERATOR,operator(hd(Data))}};
determine_token(Data,[PatternAndHandler|Rest]) ->
  {Pattern,Handler} = PatternAndHandler, 
  case re:first_match(Data,Pattern) of
    {match,1,Length} ->
      {Match,Data1} = split(Data,Length),
      Token = Handler(Match,Data1),
      {Data1,Token};
    nomatch ->
      determine_token(Data,Rest)
  end.    
  
split(Data,Length) ->
  Token = string:substr(Data,1,Length),
  Data1 = string:substr(Data,Length+1,string:len(Data)),
  {Token,Data1}.

operator(43) -> plus;
operator(45) -> minus;
operator(47) -> divide;
operator(42) -> times;
operator(40) -> lparen;
operator(41) -> rparen;
operator(37) -> remainder;
operator(94) -> exponent;
operator(44) -> comma;
operator(60) -> less_than;
operator(62) -> greater_than;
operator([61,61]) -> equality;   % ==
operator([60,61]) -> less_than_or_equal;   % <=
operator([62,61]) -> greater_than_or_equal;   % >=
operator([33,61]) -> not_equal;   % !=
operator(X) ->  throw({sheet,tokenizer,string:concat("Invalid token: ",[X])}).

token_to_list({?NUMBER,I})  when is_integer(I) -> integer_to_list(I);
token_to_list({?NUMBER,I})  when is_float(I) -> float_to_list(I);
token_to_list({?STRING,S}) -> S.
