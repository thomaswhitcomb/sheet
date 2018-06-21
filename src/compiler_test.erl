-module(compiler_test).
-include("token.hrl").
-export([
  test_get_variables1/0
	,test_get_variables2/0
	,test_get_variables3/0
	,test_get_variables4/0
	,test_get_variables5/0
	,test_get_variables6/0
  ,test_function_integer_compile/0
  ,test_function_compile1/0
  ,test_function_compile2/0
  ,test_function_compile3/0
  ,test_function_compile4/0
  ,test_function_compile5/0
  ,test_function_compile6/0
  ,test_text01/0
]).

test_get_variables1() ->
  Postfix = compiler:compile("3+a1+5"),
	Variables = compiler:get_variables(Postfix),
	erltest:assert_true(length(Variables) == 1),
	erltest:assert_true(Variables == [a1]).

test_get_variables2() ->
  Postfix = compiler:compile("3+a1+a2+7+a787"),
	Variables = compiler:get_variables(Postfix),
	erltest:assert_true(length(Variables) == 3),
	erltest:assert_true(Variables == [a1,a2,a787]).

test_get_variables3() ->
  Postfix = compiler:compile("3+a1+a2+7+a2"),
	Variables = compiler:get_variables(Postfix),
	erltest:assert_true(length(Variables) == 2),
	erltest:assert_true(Variables == [a1,a2]).

test_get_variables4() ->
  Postfix = compiler:compile("3+7"),
	Variables = compiler:get_variables(Postfix),
	erltest:assert_true(length(Variables) == 0).
  
test_get_variables5() ->
  Postfix = compiler:compile("sum(a1:e1)"),
	Variables = compiler:get_variables(Postfix),
	erltest:assert_true(length(Variables) == 5),
	erltest:assert_true(Variables == [a1,b1,c1,d1,e1]).
  
test_get_variables6() ->
  Postfix = compiler:compile("sum(a1:a10)"),
	Variables = compiler:get_variables(Postfix),
	erltest:assert_true(length(Variables) == 10),
	erltest:assert_true(Variables == [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10]).
  
test_function_integer_compile() ->
  Postfix = compiler:compile("5"),
	erltest:assert_true(Postfix == [{?NUMBER,5}]).
  
test_function_compile1() ->
  Postfix = compiler:compile("square(5+2)+3"),
	erltest:assert_true(Postfix == [{?NUMBER,5},{?NUMBER,2},{?OPERATOR,plus},{?FUNCTION,square},{?NUMBER,3},{?OPERATOR,plus}]).

test_function_compile2() ->
  Postfix = compiler:compile("2+square(5+2)+3"),
	erltest:assert_true(Postfix == [{?NUMBER,2},{?NUMBER,5},{?NUMBER,2},{?OPERATOR,plus},{?FUNCTION,square},{?OPERATOR,plus},{?NUMBER,3},{?OPERATOR,plus}]).

test_function_compile3() ->
  Postfix = compiler:compile("square(5*2)*3"),
	erltest:assert_true(Postfix == [{?NUMBER,5},{?NUMBER,2},{?OPERATOR,times},{?FUNCTION,square},{?NUMBER,3},{?OPERATOR,times}]).

test_function_compile4() ->
  Postfix = compiler:compile("(1+2)*3+square(5*2)*3"),
	erltest:assert_true(Postfix == [{?NUMBER,1},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,3},{?OPERATOR,times},{?NUMBER,5},{?NUMBER,2},{?OPERATOR,times},{?FUNCTION,square},{?NUMBER,3},{?OPERATOR,times},{?OPERATOR,plus}]).

test_function_compile5() ->
  Postfix = compiler:compile("(1+2)*(3+square(5*2))*3"),
	erltest:assert_true(Postfix == [{?NUMBER,1},{?NUMBER,2},{?OPERATOR,plus},{?NUMBER,3},{?NUMBER,5},{?NUMBER,2},{?OPERATOR,times},{?FUNCTION,square},{?OPERATOR,plus},{?OPERATOR,times},{?NUMBER,3},{?OPERATOR,times}]).

test_function_compile6() ->
  Postfix = compiler:compile("ifthenelse(a1,5,6)"),
	erltest:assert_true(Postfix == [{?VARIABLE,a1},{?NUMBER,5},{?NUMBER,6},{?FUNCTION,ifthenelse}]).

test_text01() ->
  Postfix = compiler:compile("\"this is some text\""),
	erltest:assert_true(Postfix == [{?STRING,"this is some text"}]).
