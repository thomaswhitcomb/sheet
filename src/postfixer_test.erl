-module(postfixer_test).
-include("token.hrl").
-export([
  test_integer_plus_integer/0
  ,test_integer_plus_integer_times_integer/0
  ,test_integer_times_integer_plus_integer/0
  ,test_integer_times_integer_plus_integer_with_parens/0
  ,test_integer_times_integer_plus_integer_with1LParen/0
  ,test_integer_times_integer_plus_integer_with1RParen/0
  ,test_lParenIntegerRParen/0
  ,test_just_times/0
  ,test_function_with_arity/0
  ,test_function_with_complex_arity/0
  ,test_equality/0
  ,test_not_equality/0
  ,test_less_than/0
  ,test_greater_than/0
  ,test_greater_than_or_equal/0
  ,test_less_than_or_equal/0
  ,test_function_with_list0/0
  ,test_function_with_list1/0
]).

test_integer_plus_integer() ->
  Postfix = postfixer:postfix([{?NUMBER,1},{?OPERATOR,plus},{?NUMBER,2}]),
	erltest:assert_true(Postfix == [{?NUMBER,1},{?NUMBER,2},{?OPERATOR,plus}]).

test_integer_plus_integer_times_integer() ->
  Postfix = postfixer:postfix([{?NUMBER,1},{?OPERATOR,plus},{?NUMBER,2},{?OPERATOR,times},{?NUMBER,3}]),
	erltest:assert_true(Postfix == [{?NUMBER,1},{?NUMBER,2},{?NUMBER,3},{?OPERATOR,times},{?OPERATOR,plus}]).

test_integer_times_integer_plus_integer() ->
  Postfix = postfixer:postfix(
	  [{?NUMBER,1},{?OPERATOR,times},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,3}]),
	erltest:assert_true(Postfix == [{?NUMBER,1},{?NUMBER,2},{?OPERATOR,times},{?NUMBER,3},{?OPERATOR,plus}]).

test_integer_times_integer_plus_integer_with_parens() ->
  Postfix = postfixer:postfix(
	  [{?NUMBER,1},{?OPERATOR,times},{?OPERATOR,lparen},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,3},{?OPERATOR,rparen}]),
	erltest:assert_true(Postfix == [{?NUMBER,1},{?NUMBER,2},{?NUMBER,3},{?OPERATOR,plus},{?OPERATOR,times}]).

test_integer_times_integer_plus_integer_with1LParen() ->
  try 
    postfixer:postfix( [{?NUMBER,1},{?OPERATOR,times},{?OPERATOR,lparen},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,3}])
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,postfixer,"Unbalanced parens - extra left one"} -> erltest:pass()

	end.
test_integer_times_integer_plus_integer_with1RParen() ->
  try
    postfixer:postfix( [{?NUMBER,1},{?OPERATOR,times},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,3},{?OPERATOR,rparen}])
	of
	  _ -> erltest:fail()
  catch
	  throw:{sheet,postfixer,"Unbalanced parens - extra right one"} -> erltest:pass()
	end.	

test_lParenIntegerRParen() ->
    Postfix = postfixer:postfix(
	  [{?OPERATOR,lparen},{?NUMBER,1},{?OPERATOR,rparen}]),
	erltest:assert_true(Postfix == [{?NUMBER,1}]).


test_just_times() ->
  Postfix = postfixer:postfix(
	  [{?OPERATOR,times}]),
	erltest:assert_true(Postfix == [{?OPERATOR,times}]).


test_function_with_arity() ->
  Postfix = postfixer:postfix(
	[{?FUNCTION,ifthenelse},{?OPERATOR,lparen},{?NUMBER,1},{?NUMBER,2},{?NUMBER,3},{?OPERATOR,rparen}]),
	erltest:assert_true(Postfix == [{?NUMBER,1},{?NUMBER,2},{?NUMBER,3},{?FUNCTION,ifthenelse}]).

test_function_with_complex_arity() ->
  Tokens = tokenizer:tokenize("ifthenelse(2+3/1,3*4/2,15)"),
	Postfix = postfixer:postfix(Tokens),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?NUMBER,3},{?NUMBER,1},{?OPERATOR,divide},{?OPERATOR,plus},{?NUMBER,3},{?NUMBER,4},{?OPERATOR,times},{?NUMBER,2},{?OPERATOR,divide},{?NUMBER,15},{?FUNCTION,ifthenelse}]).

test_equality() ->
  Tokens = tokenizer:tokenize("(2+3)*1 == 15+5"),
	Postfix = postfixer:postfix(Tokens),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?NUMBER,3},{?OPERATOR,plus},{?NUMBER,1},{?OPERATOR,times},{?NUMBER,15},{?NUMBER,5},{?OPERATOR,plus},{?OPERATOR,equality}]).

test_not_equality() ->
  Tokens = tokenizer:tokenize("(2+3)*1 != 15+5"),
	Postfix = postfixer:postfix(Tokens),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?NUMBER,3},{?OPERATOR,plus},{?NUMBER,1},{?OPERATOR,times},{?NUMBER,15},{?NUMBER,5},{?OPERATOR,plus},{?OPERATOR,not_equal}]).

test_less_than() ->
  Tokens = tokenizer:tokenize("(2+3)*1 < 15+5"),
	Postfix = postfixer:postfix(Tokens),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?NUMBER,3},{?OPERATOR,plus},{?NUMBER,1},{?OPERATOR,times},{?NUMBER,15},{?NUMBER,5},{?OPERATOR,plus},{?OPERATOR,less_than}]).

test_greater_than() ->
  Tokens = tokenizer:tokenize("(2+3)*1 > 15+5"),
	Postfix = postfixer:postfix(Tokens),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?NUMBER,3},{?OPERATOR,plus},{?NUMBER,1},{?OPERATOR,times},{?NUMBER,15},{?NUMBER,5},{?OPERATOR,plus},{?OPERATOR,greater_than}]).

test_greater_than_or_equal() ->
  Tokens = tokenizer:tokenize("(2+3)*1 >= 15+5"),
	Postfix = postfixer:postfix(Tokens),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?NUMBER,3},{?OPERATOR,plus},{?NUMBER,1},{?OPERATOR,times},{?NUMBER,15},{?NUMBER,5},{?OPERATOR,plus},{?OPERATOR,greater_than_or_equal}]).

test_less_than_or_equal() ->
  Tokens = tokenizer:tokenize("(2+3)*1 <= square(15+5)"),
	Postfix = postfixer:postfix(Tokens),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?NUMBER,3},{?OPERATOR,plus},{?NUMBER,1},{?OPERATOR,times},{?NUMBER,15},{?NUMBER,5},{?OPERATOR,plus},{?FUNCTION,square},{?OPERATOR,less_than_or_equal}]).

test_function_with_list0() ->
  Tokens = tokenizer:tokenize("2+avg(a1:c1)*3"),
	Postfix = postfixer:postfix(Tokens),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?LIST,{"a",1},{"c",1}},{?FUNCTION,avg},{?NUMBER,3},{?OPERATOR,times},{?OPERATOR,plus}]).

test_function_with_list1() ->
  Tokens = tokenizer:tokenize("2+(avg(a1:c1))*3"),
	Postfix = postfixer:postfix(Tokens),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?LIST,{"a",1},{"c",1}},{?FUNCTION,avg},{?NUMBER,3},{?OPERATOR,times},{?OPERATOR,plus}]).
