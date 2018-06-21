-module(sheet_server_test).
-export([
  test_start_stop/0
  ,test_simple_set1/0
  ,test_simple_set2/0
  ,test_simple_set_get1/0
  ,test_simple_set_get2/0
  ,test_simple_set_get3/0
]).

test_start_stop() ->
  Pid = sheet_server:start(),
	Ok = sheet_server:stop(Pid),
	erltest:assert_true(ok == Ok).

test_simple_set1() ->
  Pid = sheet_server:start(),
	Ok = sheet_server:set(Pid,a1,"5"),
	Stop = sheet_server:stop(Pid),
	erltest:assert_true(Stop == ok),
	erltest:assert_true(ok == Ok).

test_simple_set2() ->
  Pid = sheet_server:start(),
	Ok = sheet_server:set(Pid,a1,"5+3"),
	Stop = sheet_server:stop(Pid),
	erltest:assert_true(Stop == ok),
	erltest:assert_true(ok == Ok).

test_simple_set_get1() ->
  Pid = sheet_server:start(),
	Ok = sheet_server:set(Pid,a1,"5"),
	{Ok,Value} = sheet_server:get(Pid,a1),
	Stop = sheet_server:stop(Pid),
	erltest:assert_true(Stop == ok),
	erltest:assert_true("5" == Value),
	erltest:assert_true(Ok == ok).

test_simple_set_get2() ->
  Pid = sheet_server:start(),
	Ok = sheet_server:set(Pid,a1,"5+3-(1)"),
	{Ok,Value} = sheet_server:get(Pid,a1),
	Stop = sheet_server:stop(Pid),
	erltest:assert_true(Stop == ok),
	erltest:assert_true("7" == Value),
	erltest:assert_true(Ok == ok).

test_simple_set_get3() ->
  Pid = sheet_server:start(),
	sheet_server:set(Pid,a1,"(1+2)*(3+square(5*2))*3"),
	{ok,One} = sheet_server:get(Pid,a1),
	Stop = sheet_server:stop(Pid),
	erltest:assert_true(Stop == ok),
	erltest:assert_true(One == "927").
