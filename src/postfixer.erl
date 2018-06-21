-module(postfixer).
-include("token.hrl").
-export([
  postfix/1
]).

postfix(L) ->
  postfix1(L,[]).
postfix1([],Stack) -> 
  empty_stack(Stack);

postfix1([{?VARIABLE,Token}|Rest],Stack) ->
  [{?VARIABLE,Token}|postfix1(Rest,Stack)];

postfix1([{?STRING,Token}|Rest],Stack) ->
  [{?STRING,Token}|postfix1(Rest,Stack)];

postfix1([{?NUMBER,Token}|Rest],Stack) ->
  [{?NUMBER,Token}|postfix1(Rest,Stack)];

postfix1([{?LIST,Min,Max}|Rest],Stack) ->
  [{?LIST,Min,Max}|postfix1(Rest,Stack)];

postfix1([{Type,Token}|Rest],Stack) when (Type == ?OPERATOR) or (Type == ?FUNCTION) ->
  case length(Stack) of
    % Stack is empty, if input token is ')' then an error otherwise
    % push the token on the stack
    0 ->
      case Token of
        rparen ->
          throw({sheet,postfixer,"Unbalanced parens - extra right one"});
        _ ->
          % push the token
          postfix1(Rest,[{Type,Token}|Stack])
      end;  

    % Stack not empty  
    _ ->
      case Token of
        % current input token is a lparen
        % push it on the stack
        lparen ->
          % push the token
          postfix1(Rest,[{Type,Token}|Stack]);
        % current input token is a rparen
        rparen ->
          case hd(Stack) of
            {?OPERATOR,lparen} ->
              % remove lparen from stack
              [_|NewStack] = Stack,
              postfix1(Rest,NewStack);
            _ ->  
              % pop the operator from the stack
              [Top|NewStack] = Stack,
              [Top|postfix1([{Type,Token}|Rest],NewStack)]
          end;
        _ ->
          case precedence({Type,Token}) =< precedence(hd(Stack)) of
            true -> 
              % pop the operator from the stack
              [Top|NewStack] = Stack,
              %[Top|postfix1([{Type,Token}|Rest],NewStack)];
              [Top|postfix1([{Type,Token}|Rest],NewStack)];
            false ->
              % push the token if it is not a comma
              % comma, which has the lowest precedence causes the operator stack to be drained
              % and the comma is not put on the stack.
              case Token of
                comma -> 
                  postfix1(Rest,Stack);
                _ -> 
                  postfix1(Rest,[{Type,Token}|Stack])
              end  
          end  
      end    
  end.

empty_stack([]) -> [];
empty_stack([{?OPERATOR,lparen}|_]) ->
  throw({sheet,postfixer,"Unbalanced parens - extra left one"});
empty_stack([H|T]) ->
  [H|empty_stack(T)].


precedence({?OPERATOR,lparen}) -> -1;
precedence({?OPERATOR,exponent}) -> 10;
precedence({?OPERATOR,remainder}) -> 8;
precedence({?OPERATOR,divide}) -> 8;
precedence({?OPERATOR,times}) -> 8;
precedence({?OPERATOR,minus}) -> 6;
precedence({?OPERATOR,plus}) -> 6;
precedence({?OPERATOR,less_than}) -> 4 ;
precedence({?OPERATOR,greater_than}) -> 4 ;
precedence({?OPERATOR,less_than_or_equal}) -> 4 ;
precedence({?OPERATOR,greater_than_or_equal}) -> 4 ;
precedence({?OPERATOR,equality}) -> 3 ;
precedence({?OPERATOR,not_equal}) -> 3 ;
precedence({?OPERATOR,comma}) -> 1;
precedence({?FUNCTION,_})  -> 15  .
