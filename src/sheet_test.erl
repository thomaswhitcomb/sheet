-module(sheet_test).
-include("token.hrl").
-export([
  test_simple_set_get1/0
  ,test_simple_set_get2/0
  ,test_simple_set_get3/0
  ,test_simple_set_get4/0
  ,test_undefined1/0
  ,test_cache_hit/0
  ,test1DefaultVariable/0
  ,test3Variable/0
  ,test_multi_level_variable/0
  ,test_cells1/0
  ,test_calc1/0
  ,test_get_formula/0
  ,test_formula01/0
	,test_clear01/0
	,test_clear02/0
	,test_circularity01/0
	,test_circularity02/0
	,test_circularity03/0
	,test_circularity04/0
	,test_circularity05/0
	,test_circularity06/0
	,test_circularity07/0
	,test_circularity08/0
	,test_circularity09/0
	,test_circularity10/0
	,test_circularity11/0
	,test_circularity12/0
	,test_mix_mode_addition/0
	,test_load0/0
	,test_load1/0
	,test_load2/0
	,test_load3/0
	,test_load4/0
	,test_load5/0
	,test_load6/0
	,test_load7/0
	,test_load8/0
]).

test_simple_set_get1() ->
	Sheet = sheet:set(sheet:new(),a1,"5"),
	{_,Value} = sheet:get(Sheet,a1),
	erltest:assert_true("5" == Value).

test_simple_set_get2() ->
	Sheet = sheet:set(sheet:new(),a1,"5+3-(1)"),
	{_,Value} = sheet:get(Sheet,a1),
	erltest:assert_true("7" == Value).

test_simple_set_get3() ->
	Sheet = sheet:set(sheet:new(),a1,"(1+2)*(3+square(5*2))*3"),
	{_,Value} = sheet:get(Sheet,a1),
	erltest:assert_true(Value == "927").

test_simple_set_get4() ->
	Sheet = sheet:set(sheet:new(),a1,"\"this is a string\""),
	{_,Value} = sheet:get(Sheet,a1),
	erltest:assert_true(Value == "this is a string").

test_undefined1() ->
	{_,Value} = sheet:get(sheet:new(),a1),
	erltest:assert_true(Value == "0").

test_cache_hit() ->
	Sheet = sheet:set(sheet:new(),a1,"5"),
	{Sheet1,V1} = sheet:get(Sheet,a1),
	erltest:assert_true(V1 == "5"),
	{Sheet2,V2} = sheet:get(Sheet1,a1),
	erltest:assert_true(V2 == "5"),
	{_Sheet3,V3} = sheet:get(Sheet2,a1),
	erltest:assert_true(V3 == "5"),
	erltest:pass().

test1DefaultVariable() ->
	{_,Value} = sheet:get(sheet:new(),a1),
	erltest:assert_true(Value == "0").

test3Variable() ->
	Sheet = sheet:set(sheet:new(),a1,"1+2"),
	Sheet1 = sheet:set(Sheet,a2,"2"),
	Sheet2 = sheet:set(Sheet1,a3,"3"),
	Sheet3 = sheet:set(Sheet2,a4,"(a1)*(3+square(5*a2))*a3"),
	{_Sheet4,V1} = sheet:get(Sheet3,a4),
	erltest:assert_true(V1 == "927").

test_multi_level_variable() ->
	Sheet = sheet:set(sheet:new(),a1,"1+2"),
	Sheet1 = sheet:set(Sheet,a2,"2+a1"),
	Sheet2 = sheet:set(Sheet1,a3,"3+a2*a1"),
	{_Sheet4,V1} = sheet:get(Sheet2,a3),
	erltest:assert_true(V1 == "18").

test_cells1() ->
	Sheet = sheet:set(sheet:new(),a1,"1+2"),
	Sheet1 = sheet:set(Sheet,a2,"5+a1"),
	List = sheet:cells(Sheet1),
	List1 = lists:sort(List),
	erltest:assert_true(List1 == [{a1,"1+2","3"},{a2,"5+a1","8"}]).

test_calc1() ->
	Sheet = sheet:set(sheet:new(),a1,"1+2"),
	Sheet1 = sheet:set(Sheet,a2,"5+a1"),
	Sheet2 = sheet:set(Sheet1,d8,"a1+(a2*5)-(a2)"),
	Sheet3 = sheet:calc(Sheet2),
	{_Cells,Cache} = Sheet3,
	List = dict:to_list(Cache),
	List1 = lists:sort(List),
	erltest:assert_true(List1 == [{a1,{?NUMBER,3}},{a2,{?NUMBER,8}},{d8,{?NUMBER,35}}]).

test_get_formula() ->
	Sheet = sheet:set(sheet:new(),a1,"1+2"),
	Sheet1 = sheet:set(Sheet,a2,"5+a1"),
	Formula1 = sheet:get_formula(Sheet1,a2),
	erltest:assert_true(Formula1 == "5+a1" ),
	erltest:assert_true(sheet:get_formula(Sheet1,a1) == "1+2" ).

test_formula01() ->
	Sheet = sheet:set(sheet:new(),a1,"square(5)*2+1"),
	{_Sheet4,Value} = sheet:get(Sheet,a1),
	erltest:assert_true(Value == "51" ).

test_clear01() ->
	Sheet = sheet:set(sheet:new(),a1,"5"),
	Sheet1 = sheet:clear(Sheet,a1),
	{_,Value} = sheet:get(Sheet1,a1),
	erltest:assert_true("0" == Value).

test_clear02() ->
	Sheet = sheet:set(sheet:new(),a1,"5+2+3"),
	{Sheet1,Value1} = sheet:get(Sheet,a1),
	erltest:assert_true("10" == Value1),
	Sheet2 = sheet:clear(Sheet1,a1),
	{_,Value2} = sheet:get(Sheet2,a1),
	erltest:assert_true("0" == Value2).

test_circularity01() ->
  try
	  sheet:set(sheet:new(),a1,"5+2+a1")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity02() ->
	Sheet =  sheet:set(sheet:new(),a1,"a2"),
  try
	  sheet:set(Sheet,a2,"a1")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity03() ->
	Sheet =  sheet:set(sheet:new(),a1,"a2"),
	Sheet1 =  sheet:set(Sheet,a2,"a3"),
  try
	  sheet:set(Sheet1,a3,"a1")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity04() ->
	Sheet =  sheet:set(sheet:new(),a1,"a2+a2"),
	Sheet1 =  sheet:set(Sheet,a2,"a3+a3"),
  try
	  sheet:set(Sheet1,a3,"a1+a2")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity05() ->
	Sheet =  sheet:set(sheet:new(),a1,"2"),
	Sheet1 =  sheet:set(Sheet,a2,"a4"),
	Sheet2 =  sheet:set(Sheet1,a3,"a2"),
  try
	  sheet:set(Sheet2,a4,"a3")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity06() ->
	Sheet =  sheet:set(sheet:new(),a0,"2"),
	Sheet1 =  sheet:set(Sheet,b0,"a0"),
	Sheet2 =  sheet:set(Sheet1,c0,"a0"),
	Sheet3 =  sheet:set(Sheet2,a1,"2-(-3)"),
	Sheet4 =  sheet:set(Sheet3,d3,"b0+c0"),
  try
	  sheet:set(Sheet4,b1,"b0+d3")
	of
	  _ -> erltest:pass()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:fail()
	end.	

test_circularity07() ->
	Sheet =  sheet:set(sheet:new(),a0,"2"),
	Sheet1 =  sheet:set(Sheet,b0,"a0"),
	Sheet2 =  sheet:set(Sheet1,c0,"a0+b1"),
	Sheet3 =  sheet:set(Sheet2,a1,"2-(-3)"),
	Sheet4 =  sheet:set(Sheet3,d3,"b0+c0"),
  try
	  sheet:set(Sheet4,b1,"b0+d3")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity08() ->
	Sheet =  sheet:set(sheet:new(),d3,"b0+c0"),
	Sheet1 =  sheet:set(Sheet,a1,"2-(-3)"),
	Sheet2 =  sheet:set(Sheet1,c0,"a0+b1"),
	Sheet3 =  sheet:set(Sheet2,b0,"a0"),
	Sheet4 =  sheet:set(Sheet3,a0,"2"),
  try
	  sheet:set(Sheet4,b1,"b0+d3")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity09() ->
	Sheet =  sheet:set(sheet:new(),a0,"1"),
	Sheet1 =  sheet:set(Sheet,a1,"2"),
	Sheet2 =  sheet:set(Sheet1,a2,"3"),
	Sheet3 =  sheet:set(Sheet2,a3,"4"),
	Sheet4 =  sheet:set(Sheet3,a4,"5"),
	Sheet5 =  sheet:set(Sheet4,a5,"6"),
  try
	  sheet:set(Sheet5,a6,"sum(a0:a6)")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity10() ->
	Sheet =  sheet:set(sheet:new(),a0,"1"),
	Sheet1 =  sheet:set(Sheet,b0,"2"),
	Sheet2 =  sheet:set(Sheet1,c0,"3"),
	Sheet3 =  sheet:set(Sheet2,d0,"4"),
	Sheet4 =  sheet:set(Sheet3,e0,"5"),
	Sheet5 =  sheet:set(Sheet4,f0,"6"),
  try
	  sheet:set(Sheet5,g0,"sum(a0:h0)")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity11() ->
  try
	  sheet:set(sheet:new(),a2,"avg(a0:a3)")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_circularity12() ->
  try
	  sheet:set(sheet:new(),e2,"avg(a2:e2)")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,circularity,_Message} -> erltest:pass()
	end.	

test_mix_mode_addition() ->
	Sheet =  sheet:set(sheet:new(),a0,"\"abc\""),
	Sheet1 =  sheet:set(Sheet,b0,"2+a0"),
	{_,_Value} =  sheet:get(Sheet1,b0),
	erltest:assert_true(true == true).

test_load0() ->
  P = sheet:carve_into_properties(lists:flatten(io_lib:format("a=12~n",[]))),
	erltest:assert_true(P == [{a,"12"}]).

test_load1() ->
  P = sheet:carve_into_properties(lists:flatten(io_lib:format("a=12~nb=13~n",[]))),
	erltest:assert_true(P == [{a,"12"},{b,"13"}]).

test_load2() ->
  P = sheet:carve_into_properties(lists:flatten(io_lib:format("a=12+cd+5 ~nb=13-5 ~n",[]))),
	erltest:assert_true(P == [{a,"12+cd+5"},{b,"13-5"}]).


test_load3() ->
  Sheet1 = sheet:load(sheet:new(),lists:flatten(io_lib:format("a=12+5 ~nb=13-(5+5) ",[]))),
	{Sheet2,Value1} = sheet:get(Sheet1,a),
	{_Sheet3,Value2} = sheet:get(Sheet2,b),
	erltest:assert_true(Value1 == "17"),
	erltest:assert_true(Value2 == "3").


test_load4() ->
  Sheet1 = sheet:load(sheet:new(),lists:flatten(io_lib:format("a=12+5 ~nb=13-(5+5)~n   ",[]))),
	{Sheet2,Value1} = sheet:get(Sheet1,a),
	{_Sheet3,Value2} = sheet:get(Sheet2,b),
	erltest:assert_true(Value1 == "17"),
	erltest:assert_true(Value2 == "3").


test_load5() ->
  try
    sheet:load(sheet:new(),lists:flatten(io_lib:format("a=12+5 ~nb=13-(5+5~n   ",[])))
	of	
	  _ -> erltest:fail()
	catch
	  throw:{sheet,compiler,_} -> erltest:pass()
	end.

test_load6() ->
  try
    sheet:load(sheet:new(),lists:flatten(io_lib:format("a=12+5 b=13-(5+5~n   ",[])))
	of	
	  _ -> erltest:fail()
	catch
	  throw:{sheet,tokenizer,_} -> erltest:pass()
	end.

test_load7() ->
    Sheet = sheet:new(),
    Sheet1 = sheet:load(sheet:new(),""),
	  erltest:assert_true(Sheet1 == Sheet).


test_load8() ->
  Sheet1 = sheet:load(sheet:new(),lists:flatten(io_lib:format("a =12+5 ~nb=13-(5+5)~n   ",[]))),
	{Sheet2,Value1} = sheet:get(Sheet1,a),
	{_Sheet3,Value2} = sheet:get(Sheet2,b),
	io:format("value1 ~s Value2 ~s~n",[Value1,Value2]),
	erltest:assert_true(Value1 == "17"),
	erltest:assert_true(Value2 == "3").
