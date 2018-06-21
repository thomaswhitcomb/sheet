-module(performance_test).
-include("token.hrl").
-export([
  time_simple_set/0
	,time_complex_set/0
	,time_complex_set_with_function_calls/0
]).


 time_simple_set() ->
  Sheet = sheet:new(),
	sheet:set(Sheet,a2,"10"),
 	erltest:assert_true(1==1).

 time_complex_set() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a2,"10"),
  sheet:set(Sheet1,a1,"((2+3)*(3)+2)/(a2*5+232)-(434)*589/3"),
 	erltest:assert_true(1==1).

 time_complex_set_with_function_calls() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a2,"10"),
  sheet:set(Sheet1,a1,"((2+3)*(3)+2)/(a2*5+square(3))+square(square(2))"),
 	erltest:assert_true(1==1).
