-module(functions_test).
-include("token.hrl").
-export([
  test_square/0
  ,test_sin0/0
  ,test_sin1/0
  ,test_trunc/0
  ,test_round/0
  ,test_avg0/0
  ,test_avg1/0
]).

test_square() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a1,"square(5)"),
	{_,Value} = sheet:get(Sheet1,a1),
	erltest:assert_true(Value == "25").

test_sin0() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a1,"sin(0)"),
	{_,Value} = sheet:get(Sheet1,a1),
	erltest:assert_true(list_to_float(Value) == 0).

test_sin1() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a1,"sin(1)"),
	{_,Value} = sheet:get(Sheet1,a1),
	erltest:assert_true(string:substr(Value,1,11) == "8.414709848").

test_trunc() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a1,"trunc(sin(1))"),
	{_,Value} = sheet:get(Sheet1,a1),
	%io:format("truc(sin(1)) ~s~n",[Value]),
	erltest:assert_true(Value == "0").

test_round() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a1,"round(sin(1)+4)"),
	{_,Value} = sheet:get(Sheet1,a1),
	%io:format("truc(sin(1)) ~s~n",[Value]),
	erltest:assert_true(Value == "5").

test_avg0() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a1,"1"),
	Sheet2 = sheet:set(Sheet1,a2,"2"),
	Sheet3 = sheet:set(Sheet2,a3,"3"),
	Sheet4 = sheet:set(Sheet3,a4,"4"),
	Sheet5 = sheet:set(Sheet4,a5,"5"),
	Sheet6 = sheet:set(Sheet5,a6,"avg(a1:a5)"),
	{_,Value} = sheet:get(Sheet6,a6),
	erltest:assert_true(string:substr(Value,1,6) == "3.0000").

test_avg1() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a1,"1"),
	Sheet2 = sheet:set(Sheet1,b1,"2"),
	Sheet3 = sheet:set(Sheet2,c1,"3"),
	Sheet4 = sheet:set(Sheet3,d1,"4"),
	Sheet5 = sheet:set(Sheet4,e1,"5"),
	Sheet6 = sheet:set(Sheet5,f1,"avg(a1:e1)"),
	{_,Value} = sheet:get(Sheet6,f1),
	erltest:assert_true(string:substr(Value,1,6) == "3.0000").









