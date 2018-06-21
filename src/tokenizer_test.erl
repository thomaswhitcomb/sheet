-module(tokenizer_test).
-include("token.hrl").
-export([
  test_string1/0
  ,test_variable1/0
  ,test_variable2/0
  ,test_variable3/0
  ,test_integer1/0
  ,test_integer2/0
  ,test_blanks1/0
  ,test_blanks2/0
  ,test_blanks3/0
  ,test_math1/0
  ,test_math2/0
  ,test_float/0
  ,test_equality/0
  ,test_less_than_or_equal/0
  ,test_greater_than_or_equal/0
  ,test_not_equal/0
  ,test_less_than/0
  ,test_greater_than/0
  ,test_list01/0
  ,test_list02/0
  ,test_list03/0
  ,test_list04/0
  ,test_list05/0
  ,test_list06/0
]).

test_string1() ->
  Tokens = tokenizer:tokenize("\"a_bc_def\""),
	erltest:assert_true(Tokens == [{?STRING,"a_bc_def"}]).

test_variable1() ->
  Tokens = tokenizer:tokenize("A1"),
	erltest:assert_true(Tokens == [{?VARIABLE,a1}]).

test_variable2() ->
  Tokens = tokenizer:tokenize("aA1123"),
	erltest:assert_true(Tokens == [{?VARIABLE,aa1123}]).

test_variable3() ->
  Tokens = tokenizer:tokenize("ba0"),
	erltest:assert_true(Tokens == [{?VARIABLE,ba0}]).

test_integer1() ->
  Tokens = tokenizer:tokenize("12345"),
	erltest:assert_true(Tokens == [{?NUMBER,12345}]).

test_integer2() ->
  Tokens = tokenizer:tokenize("-1"),
	erltest:assert_true(Tokens == [{?NUMBER,-1}]).

test_blanks1() ->
  Tokens = tokenizer:tokenize("  -1  "),
	erltest:assert_true(Tokens == [{?NUMBER,-1}]).

test_blanks2() ->
  Tokens = tokenizer:tokenize("  aa1123  "),
	erltest:assert_true(Tokens == [{?VARIABLE,aa1123}]).

test_blanks3() ->
  Tokens = tokenizer:tokenize("  aa1 123  "),
	erltest:assert_true(Tokens == [{?VARIABLE,aa1},{?NUMBER,123}]).

test_math1() ->
  Tokens = tokenizer:tokenize("  aa1+123  "),
	erltest:assert_true(Tokens == [{?VARIABLE,aa1},{?OPERATOR,plus},{?NUMBER,123}]).

test_math2() ->
  Tokens = tokenizer:tokenize("  aa1+123 * 66 /45 "),
	erltest:assert_true(Tokens == [{?VARIABLE,aa1},{?OPERATOR,plus},{?NUMBER,123},{?OPERATOR,times},{?NUMBER,66},{?OPERATOR,divide},{?NUMBER,45}]).

test_float() ->
  Tokens = tokenizer:tokenize(" 34.5 + 23.999 "),
	erltest:assert_true(Tokens == [{?NUMBER,34.5},{?OPERATOR,plus},{?NUMBER,23.999}]).

test_equality() ->
  Tokens = tokenizer:tokenize(" 34.5 == 23.999 "),
	erltest:assert_true(Tokens == [{?NUMBER,34.5},{?OPERATOR,equality},{?NUMBER,23.999}]).

test_less_than_or_equal() ->
  Tokens = tokenizer:tokenize(" 34.5 <= 23.999 "),
	erltest:assert_true(Tokens == [{?NUMBER,34.5},{?OPERATOR,less_than_or_equal},{?NUMBER,23.999}]).

test_greater_than_or_equal() ->
  Tokens = tokenizer:tokenize(" 34.5 >= 23.999 "),
	erltest:assert_true(Tokens == [{?NUMBER,34.5},{?OPERATOR,greater_than_or_equal},{?NUMBER,23.999}]).

test_not_equal() ->
  Tokens = tokenizer:tokenize(" 34.5 != 23.999 "),
	erltest:assert_true(Tokens == [{?NUMBER,34.5},{?OPERATOR,not_equal},{?NUMBER,23.999}]).

test_greater_than() ->
  Tokens = tokenizer:tokenize(" 34.5 > 23.999 "),
	erltest:assert_true(Tokens == [{?NUMBER,34.5},{?OPERATOR,greater_than},{?NUMBER,23.999}]).

test_less_than() ->
  Tokens = tokenizer:tokenize(" 34.5 < 23.999 "),
	erltest:assert_true(Tokens == [{?NUMBER,34.5},{?OPERATOR,less_than},{?NUMBER,23.999}]).

test_list01() ->
  Tokens = tokenizer:tokenize(" a1:a10 "),
	erltest:assert_true(Tokens == [{?LIST,{"a",1},{"a",10}}]).

test_list02() ->
  Tokens = tokenizer:tokenize(" a1:f1 "),
	erltest:assert_true(Tokens == [{?LIST,{"a",1},{"f",1}}]).
	
test_list03() ->
  Tokens = tokenizer:tokenize(" g1:g1 "),
	erltest:assert_true(Tokens == [{?LIST,{"g",1},{"g",1}}]).
	
test_list04() ->
  try 
    tokenizer:tokenize(" a2:f1 ")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,tokenizer,_} -> erltest:pass()
	end.

test_list05() ->
  try 
    tokenizer:tokenize(" aa2:a5 ")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,tokenizer,_} -> erltest:pass()
	end.

test_list06() ->
  try tokenizer:tokenize(" a2:ac6 ") of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,tokenizer,_} -> erltest:pass()
	end.
