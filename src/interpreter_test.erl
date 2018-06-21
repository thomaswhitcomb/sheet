-module(interpreter_test).
-include("token.hrl").
-export([
  test_resolve_integer/0
	,test_resolve_string/0
	,test_resolve_list0/0
	,test_resolve_list1/0
  ,test_integer_integer_add/0
  ,test_integer_integer_minus/0
  ,test_integer_integer_times/0
  ,test_integer_integer_divide/0
  ,test_integer_integer_add_integer_times/0
  ,test_integer_integer_add_integer/0
  ,test_function_with_arity/0
  ,test_integer_integer/0
  ,test_integer_times/0
  ,test_integer_divide/0
  ,test_compile_interpret01/0
  ,test_compile_interpret02/0
  ,test_compile_interpret03/0
  ,test_compile_interpret04/0
  ,test_compile_interpret05/0
  ,test_compile_interpret06/0
  ,test_compile_interpret07/0
  ,test_compile_interpret08/0
  ,test_compile_interpret09/0
  ,test_compile_interpret10/0
  ,test_compile_interpret11/0
  ,test_compile_interpret12/0
  ,test_compile_interpret13/0
  ,test_compile_interpret14/0
  ,test_compile_interpret15/0
  ,test_compile_interpret16/0
  ,test_compile_interpret17/0
  ,test_compile_interpret18/0
  ,test_compile_interpret19/0
  ,test_compile_interpret20/0
  ,test_compile_interpret21/0
  ,test_compile_interpret22/0
  ,test_compile_interpret23/0
  ,test_compile_interpret24/0
  ,test_compile_interpret25/0
  ,test_compile_interpret26/0
  ,test_compile_interpret27/0
  ,test_compile_interpret28/0
  ,test_compile_interpret29/0
  ,test_compile_with_default_variable/0
]).
test_resolve_integer() ->
  Sheet = sheet:new(),
	Value = interpreter:resolve({?NUMBER,5},Sheet),
	erltest:assert_true(Value == {Sheet,{?NUMBER,5}}).

test_resolve_string() ->
  Sheet = sheet:new(),
	Value = interpreter:resolve({?STRING,"abc"},Sheet),
	erltest:assert_true(Value == {Sheet,{?STRING,"abc"}}).

test_resolve_list0() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a1,"1"),
	Sheet2 = sheet:set(Sheet1,a2,"2"),
	Sheet3 = sheet:set(Sheet2,a3,"3"),
	Sheet4 = sheet:set(Sheet3,a4,"4"),
	Sheet5 = sheet:set(Sheet4,a5,"5"),
	Value = interpreter:resolve({?LIST,{"a",1},{"a",5}},Sheet5),
	erltest:assert_true(Value == {Sheet5,{?LIST,[{?NUMBER,1},{?NUMBER,2},{?NUMBER,3},{?NUMBER,4},{?NUMBER,5}]}}).

test_resolve_list1() ->
  Sheet = sheet:new(),
	Sheet1 = sheet:set(Sheet,a1,"1"),
	Sheet2 = sheet:set(Sheet1,b1,"2"),
	Sheet3 = sheet:set(Sheet2,c1,"3"),
	Sheet4 = sheet:set(Sheet3,d1,"4"),
	Sheet5 = sheet:set(Sheet4,e1,"5"),
	Value = interpreter:resolve({?LIST,{"a",1},{"e",1}},Sheet5),
	erltest:assert_true(Value == {Sheet5,{?LIST,[{?NUMBER,1},{?NUMBER,2},{?NUMBER,3},{?NUMBER,4},{?NUMBER,5}]}}).

test_integer_integer_add() ->
  Postfix = [{?NUMBER,1},{?NUMBER,3},{?OPERATOR,plus}],
  Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,4}).

test_integer_integer_minus() ->
  Postfix = [{?NUMBER,5},{?NUMBER,3},{?OPERATOR,minus}],
  Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,2}).

test_integer_integer_times() ->
  Postfix = [{?NUMBER,5},{?NUMBER,3},{?OPERATOR,times}],
  Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,15}).

test_integer_integer_divide() ->
  Postfix = [{?NUMBER,6},{?NUMBER,3},{?OPERATOR,divide}],
  Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,2}).

test_integer_integer_add_integer_times() ->
  Postfix = [{?NUMBER,1},{?NUMBER,3},{?OPERATOR,plus},{?NUMBER,2},{?OPERATOR,times}],
  Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,8}).

test_function_with_arity() ->
	Postfix = [{?NUMBER,1},{?NUMBER,2},{?NUMBER,3},{?FUNCTION,ifthenelse}],
  Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,2}).

test_integer_integer_add_integer() ->
  Postfix = [{?NUMBER,1},{?NUMBER,3},{?OPERATOR,plus},{?NUMBER,2}],
	try
	  interpreter:run(Postfix)
	of	
	  _ -> erltest:fail()
	catch 
	  throw:{sheet,interpreter,_Message} -> erltest:pass()
	end.

test_integer_integer() ->
  Postfix = [{?NUMBER,1},{?NUMBER,3}],
	try
    interpreter:run(Postfix)
	of
	  _ -> erltest:fail()
  catch	
	  throw:{sheet,interpreter,_Message} -> erltest:pass()
	end.

test_integer_times() ->
  Postfix = [{?NUMBER,1},{?OPERATOR,times}],
	try
    interpreter:run(Postfix)
	of
	  _ -> erltest:fail()
  catch	
	  throw:{sheet,interpreter,_Message} -> erltest:pass()
	end.

test_integer_divide() ->
  Postfix = compiler:compile("5/1"),
  Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,5}).

test_compile_interpret01() ->
  Postfix = compiler:compile("2*(5+3)/2"),
	Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,8}).

test_compile_interpret02() ->
  Postfix = compiler:compile("10/(2+(5+3))"),
	Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,1}).

test_compile_interpret03() ->
  Postfix = compiler:compile("10/(2+(5+3))*(10+1)"),
	Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,11}).

test_compile_interpret04() ->
  Postfix = compiler:compile("10-(2+5-(3))-(7)"),
	Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,-1}).

test_compile_interpret05() ->
  Postfix = compiler:compile("(1+2)*(3+square(5*2))*3"),
	Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,927}).

test_compile_with_default_variable() ->
  Postfix = compiler:compile("a1"),
	Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,0}).

test_compile_interpret06() ->
  Postfix = compiler:compile("(1+((2)))*(3+square(5*square(2)))*4"),
	Value = interpreter:run(Postfix),
	erltest:assert_true(Value == {?NUMBER,4836}).

test_compile_interpret07() ->
  try
    compiler:compile("(1+((2)))*(3 4+square(5*square(2)))*4")
	of
	  _ -> erltest:fail()
	catch	
	  throw:{sheet,compiler,_Message} -> erltest:pass()
	end.	

test_compile_interpret08() ->
  try
    compiler:compile("a1-b4*b9+square 5")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,compiler,_Message} -> erltest:pass()
	end.

test_compile_interpret09() ->
  try
    compiler:compile("(5+3))")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,compiler,_Message} -> erltest:pass()
	end.
		
test_compile_interpret10() ->
  try
    compiler:compile("(5$3)")
	of
	  _ -> erltest:fail()
	catch
	  throw:{sheet,tokenizer,_Message} -> erltest:pass()
	end.	
		
test_compile_interpret11() ->
  try
    compiler:compile("4++")
	of	
	  _ -> erltest:fail()
	catch
	  throw:{sheet,compiler,_Message} -> erltest:pass()
	end.	

test_compile_interpret12() ->
  try
    compiler:compile("4r33")
	of	
	  _ -> erltest:fail()
	catch
	  throw:{sheet,compiler,_Message} -> erltest:pass()
	end.	

test_compile_interpret13() ->
  P = compiler:compile("3.14*1"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,3.14} ).

test_compile_interpret14() ->
  P = compiler:compile("3.14+1.86+1"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,6.000} ).

test_compile_interpret15() ->
  P = compiler:compile("ifthenelse(2+3/1,3*4/2,15)"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,6} ).

test_compile_interpret16() ->
  P = compiler:compile("3+ifthenelse(2+3/1,3*4/2,15)*2"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,15} ).

test_compile_interpret17() ->
  P = compiler:compile("(7-(3))/2==10/5"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,1} ).

test_compile_interpret18() ->
  P = compiler:compile("7+3==10/5"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,0} ).

test_compile_interpret19() ->
  P = compiler:compile("(7+3)/2!=10/1"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,1} ).

test_compile_interpret20() ->
  P = compiler:compile("(7+3)/2!=10/2"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,0} ).

test_compile_interpret21() ->
  P = compiler:compile("square(5)+4 < 30-(1)"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,0} ).

test_compile_interpret22() ->
  P = compiler:compile("square(1)+4 < 30-(1)"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,1} ).

test_compile_interpret23() ->
  P = compiler:compile("square(5)+4 <= 30-(1)"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,1} ).

test_compile_interpret24() ->
  P = compiler:compile("square(6)+4 <= 30-(1)"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,0} ).

test_compile_interpret25() ->
  P = compiler:compile("square(5)+5 > 30-(1)"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,1} ).

test_compile_interpret26() ->
  P = compiler:compile("square(5)+5 > 30"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,0} ).

test_compile_interpret27() ->
  P = compiler:compile("square(5)+5 >= 30+0+0+0+0+0"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,1} ).

test_compile_interpret28() ->
  P = compiler:compile("square(1)+5 >= 30+7"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,0} ).

test_compile_interpret29() ->
  P = compiler:compile("square(5)/5"),
	Value = interpreter:run(P),
	erltest:assert_true(Value == {?NUMBER,5} ).
