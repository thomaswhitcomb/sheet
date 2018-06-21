-module(parser_test).
-include("token.hrl").
-export([
  test_integer/0
  ,test_integer_plus_integer/0
  ,test_integer_plus_integer_times_integer/0
  ,test_integer_times_integer_plus_integer/0
  ,test_integer_times_integer_plus_integer_with_parens/0
  ,test_function/0
  ,test_complex_function/0
  ,test_parse_of_integer_plus_integer/0
  ,test_parse_of_complex_function/0
  ,test_parse_of_bad_complex_function/0
  ,test_parse_of_super_complex_function/0
  ,test_parse_of_bad_super_complex_function/0
  ,test_parse_of_function_in_function/0
  ,test_parse_of_string/0
  ,test_parse_of_string_with_other_non_string1/0
  ,test_parse_of_string_with_other_non_string2/0
  ,test_parse_two_operands_no_operator/0
  ,test_parse_of_just_list0/0
  ,test_parse_of_list_in_function0/0
  ,test_parse_of_list_in_function1/0
  ,test_parse_of_list_in_function2/0
  ,test_parse_of_list_in_function3/0
  ,test_parse_of_list_in_function4/0
]).

test_integer() ->
  Result = parser:normalize([{?NUMBER,1}]),
	erltest:assert_true(Result == "t").

test_integer_plus_integer() ->
  Result = parser:normalize([{?NUMBER,1},{?OPERATOR,plus},{?NUMBER,2}]),
	erltest:assert_true(Result == "tot").

test_integer_plus_integer_times_integer() ->
  Result = parser:normalize([{?NUMBER,1},{?OPERATOR,plus},{?NUMBER,2},{?OPERATOR,times},{?NUMBER,3}]),
	erltest:assert_true(Result == "totot").
 
 test_integer_times_integer_plus_integer() ->
   Result = parser:normalize([{?NUMBER,1},{?OPERATOR,times},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,3}]),
 	erltest:assert_true(Result == "totot").
 
 test_integer_times_integer_plus_integer_with_parens() ->
   Result = parser:normalize([{?NUMBER,1},{?OPERATOR,times},{?OPERATOR,lparen},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,3},{?OPERATOR,rparen}]),
 	erltest:assert_true(Result == "to{tot}").

 test_function() ->
   Result = parser:normalize([{?FUNCTION,square},{?OPERATOR,lparen},{?NUMBER,2},{?OPERATOR,comma},{?NUMBER,3},{?OPERATOR,rparen}]),
 	erltest:assert_true(Result == "f{t,t}").
 
 test_complex_function() ->
   Result = parser:normalize([{?VARIABLE,abc},{?OPERATOR,plus},{?FUNCTION,square},{?OPERATOR,lparen},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,5},{?OPERATOR,comma},{?OPERATOR,lparen},{?NUMBER,3},{?OPERATOR,rparen},{?OPERATOR,rparen}]),
 	erltest:assert_true(Result == "tof{tot,{t}}").

test_parse_of_integer_plus_integer() ->
  Result = parser:parse([{?NUMBER,1},{?OPERATOR,plus},{?NUMBER,2}]),
	erltest:assert_true(Result == ok).

 test_parse_of_complex_function() ->
   Result = parser:parse([{?VARIABLE,abc},{?OPERATOR,plus},{?FUNCTION,square},{?OPERATOR,lparen},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,5},{?OPERATOR,comma},{?OPERATOR,lparen},{?NUMBER,3},{?OPERATOR,rparen},{?OPERATOR,rparen}]),
 	erltest:assert_true(Result == ok).

 test_parse_of_bad_complex_function() ->
   Result = parser:parse([{?VARIABLE,abc},{?OPERATOR,plus},{?FUNCTION,square},{?OPERATOR,lparen},{?NUMBER,2},{?NUMBER,5},{?OPERATOR,comma},{?OPERATOR,lparen},{?NUMBER,3},{?OPERATOR,rparen},{?OPERATOR,rparen}]),
 	erltest:assert_true(Result == fail).

 test_parse_of_super_complex_function() ->
  Tokens = tokenizer:tokenize("((2+3)*(3)+2)/(a1*5+square(3))+square(square(2))"),
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == ok).

 test_parse_of_bad_super_complex_function() ->
  Tokens = tokenizer:tokenize("((2+3)*(3)+2)/(a1*5+square(3)3)+square(square(2))"),
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == fail).

 test_parse_of_function_in_function() ->
  Tokens = tokenizer:tokenize("square(square(2))"),
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == ok).

 test_parse_of_string() ->
  Tokens = tokenizer:tokenize("\"abc\""),
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == ok).

 test_parse_of_string_with_other_non_string1() ->
  Tokens = tokenizer:tokenize("5+\"abc\""),
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == fail).

 test_parse_of_string_with_other_non_string2() ->
  Tokens = tokenizer:tokenize("(\"abc\")"),
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == fail).

 test_parse_two_operands_no_operator() ->
  Tokens = tokenizer:tokenize("5 4"),
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == fail).

 test_parse_of_just_list0() ->
  Tokens = tokenizer:tokenize("a1:c1"), % should fail.
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == fail).

 test_parse_of_list_in_function0() ->
  Tokens = tokenizer:tokenize("avg(a1:c1)"), % should succeed.
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == ok).

 test_parse_of_list_in_function1() ->
  Tokens = tokenizer:tokenize("avg(a1:c1,2)"), % should fail.
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == fail).

 test_parse_of_list_in_function2() ->
  Tokens = tokenizer:tokenize("avg(2,a1:c1)"), % should fail.
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == fail).

 test_parse_of_list_in_function3() ->
  Tokens = tokenizer:tokenize("avg(2,a1:c1)"), % should fail.
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == fail).

 test_parse_of_list_in_function4() ->
  Tokens = tokenizer:tokenize("avg(d1,a1:c1)"), % should fail.
	Result = parser:parse(Tokens),
 	erltest:assert_true(Result == fail).
